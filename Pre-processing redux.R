#BAN Pre-processing (redux)
#library
library('tidyverse')
library("rpart")
library("rpart.plot")
library("C50")
library('class')
library('mice')
library('naniar')
library('neuralnet')
library('RSNNS')
library('rattle')
library('merTools')
library("class")
library("fields")
library('caret')
#functions used
twoTtest<- function(x,y){#x is the train, y is the test
  tstat<-(mean(x)-mean(y))/sqrt(((sd(x)^2/length(x))+(sd(y)^2/length(y))))
  return(tstat)
}
min.df<-function(x,y){
  min(length(x)-1,length(y)-1)
}
twotailpvalue<-function(v1,v2){
  2*pt(v1,df= v2, lower.tail = FALSE)
}
minmax.nrom<- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#Looking back at the missing data we can see the following pattern
md.pattern(census.copy.na[,c(2,7,14)])
# if occupation is missing than workclass is missing. 942
# if native.country is missing with no impact on wether or not anyother variable is missing 284
# The remander of the missing data is 36 with multiple missing data. Since this is a small number we will discard 
# these missing data points and develope imputation models to solve as accuratly as possible for the 1,226 missing points
census.complete<- census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
census.complete.copy<-census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
#splitting the test and training
set.seed(458)
samp<- sample(15013, 1502)
census.train<- census.complete[-samp,]
census.test<- census.complete[samp,]
#making sure the split is nice
  #age 2-tail T test
age.tvalue<-twoTtest(census.train$age,census.test$age)
age.df<-min.df(census.train$age,census.test$age)
age.pvalue<-twotailpvalue(age.tvalue,age.df)
age.pvalue # good split on age

#edu
table(census.train$education)
table(census.test$education)
edu.matrix<- matrix(c(1721,195,8349,939,2286,230,788,96,219,22,148,20), nrow = 2)
chisq.test(edu.matrix) #good split on education

#workclass
table(census.train$workclass)
table(census.test$workclass)
work.matrix<- matrix(c(1934,222,9920,1082,1657,198), nrow = 2)
chisq.test(work.matrix) #good split

#C50 decision trees
  #work
x<- census.train[,-c(13:40,2,5)]
y<- census.train[,2]
work.c50<- C5.0(x,y) 
summary(work.c50)
c50.work.predict<- predict.C5.0(work.c50, census.test[,-c(13:40,2,5)])
table(census.test$workclass, c50.work.predict)
(10+1024+33)/1502 #70 percent error on the test
  #occupation
x<-census.train[,-c(13:40,2,5)]
y<-census.train[,5]
occupation.c50<-C5.0(x,y)
summary(occupation.c50)
c50.occ.predict<- predict.C5.0(occupation.c50, census.test[,-c(13:40,2,5)])
table(census.test$occupation, c50.occ.predict)
  #nationality
x<- census.train[,-c(13:40,11)]
y<- census.train[,11]
nationality.C50<- C5.0(x,y)
summary(nationality.C50)
C50.nat.predictMM<- predict.C5.0(nationality.C50, census.test[, -c(13:40,11)])
table(census.test$native.country, C50.nat.predictMM)
(29+1366)/1502 
table(census.test$native.country)
1381/1502 #basically we barily did better than just selecing north America

#CART imputation
  #work
work.cart<- rpart(workclass~education+age+sex+race+marital.status+hours.per.week+capital.gain+capital.loss+ native.country+income.class,
                  data= census.train,
                  method= 'class')
summary(work.cart) #can't split so selects private and achives 73% accuracy

  #occupation
occ.cart<- rpart(occupation~ age+ education+ marital.status+race+hours.per.week+ native.country+capital.loss+capital.gain+income.class,
                   data = census.train,
                   method = 'class')
summary(occ.cart)
rpart.plot(occ.cart) #intuitivly makes sence to split around education level.
occ.cart.predict<-predict(occ.cart, census.test, type = 'class')
table(census.test$occupation, occ.cart.predict)
(215+234+189)/1502 #thats rough 42% accuracy

  #nationality
nat.cart<-rpart(native.country~age+ education+marital.status+workclass+race+occupation+ race+ sex+capital.gain+capital.loss+hours.per.week+income.class,
                data= census.train,
                method= 'class')
summary(nat.cart)
rpart.plot(nat.cart)
nat.cart.predict<- predict(nat.cart, census.test, type= 'class')
table(census.test$native.country, nat.cart.predict)
(1365+30)/1502

#K nearest neightbor
  #workclass1
x<- census.train[,c(1,8:10,16:23)]
y<- census.test[,c(1,8:10,16:23)]
z<- census.train[,2]
work.knn<- knn(x,y, cl= z, k=19)
table(work.knn, census.test$workclass)
(7+1051+18)/1502 #after experimenting with K the base line accuracy seems to be about 71%
  #occupation
x<- census.train[,c(1,8:10,16:23)]
y<- census.test[,c(1,8:10,16:23)]
z<- census.train[,5]
occ.knn<- knn(x,y,cl= z, k=19)
table(occ.knn, census.test$occupation)
(55+230+241+104+12)/1502 #43% accuracy
  #nationality
x<- census.train[,c(1,8:10,16:23)]
y<- census.test[,c(1,8:10,16:23)]
z<- census.train[,11]
nat.knn<- knn(x,y,cl=z, k=9)
table(nat.knn,census.test$native.country) #predicts pretty much a straigh up non-accuracy

#Neural Network
  #workclass
x<- census.train[,c(1,8,9,10,16:23,29:39,2)]
y<- census.test[,c(1,8,9,10,16:23,29:39,2)]
work.nn<- nnet(workclass~., data= x, size= 10)
work.predict.nn<- predict(work.nn, y, type = 'class')
table(work.predict.nn, y$workclass)
(12+1047+25)/1502
  #occupation
x<- census.train[,c(1,8,9,10,16:23,29:39,5)]
y<- census.test[,c(1,8,9,10,16:23,29:39,5)]
occ.nn<- nnet(occupation~.,data=x, size=10)
occ.pred.nn<- predict(occ.nn,y, type= 'class')
table(occ.pred.nn, census.test$occupation)
(117+23+94+6+9)
  #nationality
x<- census.train[,c(1,8,9,10,16:31,11)]
y<- census.test[,c(1,8,9,10,16:31,11)]
nat.nn<- nnet(native.country~.,data= x, size=20)
nat.pred.nn<- predict(nat.nn,y, type='class')
table(nat.pred.nn, census.test$native.country)
#not predicting very well. 