#BAN Project, Data pre-processing (Run EDA for this to work)
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

#dealing with the missing data in workclass and occupation
census.complete<- census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
census.complete.full<-census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
#Cross Validation of test and train
set.seed(876521)   #note because we set the seed we do not the split of test and train will always be the same
census.complete<-census.complete[sample(1:nrow(census.complete), length(1:nrow(census.complete))), 1:ncol(census.complete)]
Test<-census.complete[,15]
Train<-census.complete[,c(1:14)]
Test<- decodeClassLabels(Test)
census.complete<-splitForTrainingAndTest(Train,Test, ratio = .1)
  #age two tail t test
imput.train<-as.data.frame(census.complete$inputsTrain)
  attr(imput.train$workclass, "ATT") <- NULL
  attr(imput.train$fnlwgt, "ATT") <- NULL
  attr(imput.train$education, "ATT") <- NULL
  attr(imput.train$education.num, "ATT") <- NULL
  attr(imput.train$marital.status, "ATT") <- NULL
  attr(imput.train$occupation, "ATT") <- NULL
  attr(imput.train$relationship, "ATT") <- NULL
  attr(imput.train$race, "ATT") <- NULL
  attr(imput.train$sex, "ATT") <- NULL
  attr(imput.train$capital.gain, "ATT") <- NULL
  attr(imput.train$capital.loss, 'ATT')<- NULL
  attr(imput.train$native.country, "ATT") <- NULL
imput.train$age<-as.numeric(imput.train$age)
imput.test<-as.data.frame(census.complete$inputsTest)
  attr(imput.test$workclass, "ATT") <- NULL
  attr(imput.test$fnlwgt, "ATT") <- NULL
  attr(imput.test$education, "ATT") <- NULL
  attr(imput.test$education.num, "ATT") <- NULL
  attr(imput.test$marital.status, "ATT") <- NULL
  attr(imput.test$occupation, "ATT") <- NULL
  attr(imput.test$relationship, "ATT") <- NULL
  attr(imput.test$race, "ATT") <- NULL
  attr(imput.test$sex, "ATT") <- NULL
  attr(imput.test$capital.gain, "ATT") <- NULL
  attr(imput.test$capital.loss, 'ATT')<- NULL
  attr(imput.test$native.country, "ATT") <- NULL
  imput.test$age<- as.numeric(imput.test$age)
#t tail t-test
age.tvalue<-twoTtest(imput.train$age,imput.test$age)
age.df<-min.df(imput.train$age,imput.test$age)
age.pvalue<-twotailpvalue(age.tvalue,age.df)
age.pvalue #since p value is large there is no evidence regect u1=u2 

  #Chi-square test for homogenity of proportions(edu)
imput.train.edu<- as.data.frame(table(imput.train$education))
imput.train.edu<- imput.train.edu%>%rename('Training'=Var1)
imput.train.edu<-t(imput.train.edu)
imput.train.edu<- imput.train.edu[-1,]
names(imput.train.edu)<- c("Doctoral","Graduate",'HS Grad','No HS Degree','Prof-school','Undergrad')
imput.train.edu$dataSet<-"training observed"
imput.train.edu<- as.data.frame(imput.train.edu)
imput.train.edu<- imput.train.edu[,c(7,1:6)]
imput.train.edu[,c(2:7)]<- as.numeric(imput.train.edu[,c(2:7)])
imput.train.edu[1,c(2:7)]<- c(150,786,8350,1744,222,2257)#these numbers need to be changed everytime the sample is re-run, the seed is set so all should be good
imput.train.edu[1,1]<-"training observed"
imput.train.edu$total<-sum(imput.train.edu[1,c(2:7)])
    #Now for the Test set :O 
imput.test.edu<- as.data.frame(table(imput.test$education))
imput.test.edu<- imput.test.edu%>%rename('test'=Var1)
imput.test.edu<-t(imput.test.edu)
imput.test.edu<- imput.test.edu[-1,]
names(imput.test.edu)<- c("Doctoral","Graduate",'HS Grad','No HS Degree','Prof-school','Undergrad')
imput.test.edu$dataSet<-"test observed"
imput.test.edu<- as.data.frame(imput.test.edu)
imput.test.edu<- imput.test.edu[,c(7,1:6)]
imput.test.edu[,c(2:7)]<- as.numeric(imput.test.edu[,c(2:7)])
imput.test.edu[1,c(2:7)]<- c(18,98,936,172,19,259)
imput.test.edu[1,1]<-"test observed"
imput.test.edu$total<-sum(imput.test.edu[1,c(2:7)])
    #adding a Total Row
census.complete.df<-census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
imput.total.edu<- as.data.frame(table(census.complete.df$education))
imput.total.edu<- imput.total.edu%>%rename('total'=Var1)
imput.total.edu<-t(imput.total.edu)
imput.total.edu<- imput.total.edu[-1,]
names(imput.total.edu)<- c("No HS Degree","HS Grad",'Undergrad','Graduate','Prof-school','Doctoral')
imput.total.edu$dataSet<-"total"
imput.total.edu<- as.data.frame(imput.total.edu)
imput.total.edu<- imput.total.edu[,c(7,1:6)]
imput.total.edu[,c(2:7)]<- as.numeric(imput.test.edu[,c(2:7)])
imput.total.edu[1,c(2:7)]<- c(1916,9286,2516,884,241,168)
imput.total.edu[1,1]<-"total"
imput.total.edu$total<-sum(imput.total.edu[1,c(2:7)])
  #combing the data frames
observed.edu<-rbind(imput.train.edu,imput.test.edu,imput.total.edu)
observed.edu
  #Calculating the test statistic X^2data
ob.edu<-observed.edu[-3,c(-1,-8)]
Xsq_edu<- chisq.test(ob.edu)
Xsq_edu$statistic
Xsq_edu$p.value#No evidence to reject our proportions
Xsq_edu$expected

  #Two sample Z-test for difference in proportions
ztest.sex.train<- as.data.frame(table(imput.train$sex))
ztest.sex.test<- as.data.frame(table(imput.test$sex))
x<-data.frame(Var1='total',Freq=length(imput.train$age))
ztest.sex.train<- rbind(ztest.sex.train,x)
x<- data.frame(Var1= 'total', Freq=length(imput.test$age))
ztest.sex.test<-rbind(ztest.sex.test,x)
rm(x)
x1<-ztest.sex.train[1,2]
x2<-ztest.sex.test[1,2]
n1<-ztest.sex.train[3,2]
n2<-ztest.sex.test[3,2]
p1<-((ztest.sex.train[1,2])/ztest.sex.train[3,2])
p2<-((ztest.sex.test[1,2])/ztest.sex.test[3,2])
ppooled<-(x1+x2)/(n1+n2)
sex.zdata<- (p1-p2)/sqrt((ppooled*(1-ppooled)*((1/n1)+1/n2)))
2*pnorm(abs(sex.zdata), lower.tail = FALSE)   #We have enought support to validate our train and test sets

#Decision tree C.50 to impute
  #Normalize the numerics
    #note for this instance we will disclued native.country because I forgto to get rid of ? 
min(imput.train$age)
max(imput.train$age)
imput.train$hours.per.week<-as.numeric(imput.train$hours.per.week)
imput.test$hours.per.week<-as.numeric(imput.test$hours.per.week)
min(imput.train$hours.per.week)
max(imput.train$hours.per.week)
imput.train$age.norm<- (imput.train$age-17)/(90-17)
imput.train$hrs.norm<- (imput.train$hours.per.week-1)/(99-1)
names(imput.train)
x<- imput.train[,c(4,6,8,9,10,14,15,16)]
names(x)
y<-imput.train[,c(2)]
c50.work<- C5.0(x,y)
summary(c50.work) #26% error not ideal :(
  #Lets compare against the test set:|
imput.test$age<-as.numeric(imput.test$age)
imput.test$age.norm<- (imput.test$age-17)/(90-17)
imput.test$hrs.norm<- (imput.test$hours.per.week-1)/(99-1)
names(imput.test)
x<-imput.test[,c(4,6,8,9,10,14,15,16)]
test.predict<-predict.C5.0(c50.work,x) 
summary(test.predict) 
test.actual<-imput.test$workclass
table(test.actual, test.predict)
(8+1076+19)/1502
summary(test.predict)
summary(test.actual) 
    #pretty close to the 25% error rate given in the model.
#This discludes opperations to predict workclass.
#This model is overfitting private. however, it is not agregiously overfitting but not ideal.

#Decision Tree c.5 for occupation  
names(imput.train)
x<-imput.train[,c(2,4,6,8,9,10,14,15,16)]
names(x)
y<-imput.train[,7]
occ.c50<- C5.0(x,y, trials = 3)
summary(occ.c50) #42% error... 
x<-imput.test[,c(2,4,6,8,9,10,14,15,16)]
oc.c50.predict<- predict.C5.0(occ.c50,x)
occ.test.actual<-imput.test$occupation
table(occ.test.actual,oc.c50.predict)
sum(occ.test.actual== oc.c50.predict)/1502 #accuracy

#I might need to re-think some of the catagories for these
#national.
names(imput.train)
x<-imput.train[,c(2,4,6,7,8,9,10,13,15,16)]
y<-imput.train[,14]
nat.c50<- C5.0(x,y, trials= 1)
summary(nat.c50) #6.8% error NICE, however, it seems we over selecting the North American continent. Based on the summary 
names(imput.test)
x<-imput.test[,c(2,4,6,7,8,9,10,13,15,16)]
nat.predict.c5<- predict.C5.0(nat.c50,x)
summary(nat.predict.c5)
nat.test.actual<-imput.test$native.country
table(nat.test.actual,nat.predict.c5)
 table(nat.test.actual) 
 #here we can see that are model is overfitting North America by quite a bit which
 #makes sense because it is what majority of the countries fall under North America.
 #Thus we might need to balance 
 
#CART,
    #note CART can't split train.workclass
names(imput.train)
work.cart<- rpart(workclass~education+age.norm+hrs.norm+sex+race+marital.status+occupation,
                  data= imput.train,
                  method= 'class')
  summary(work.cart)
  #due to the high percentage of people working in private the model can't split

cart.train<- rpart(occupation~ education+age.norm+hrs.norm+sex+race+marital.status, #This is a pretty awful split, smh
                   data = imput.train,
                   method = 'class')
rpart.plot(cart.train)

nat.cart<-rpart(native.country~education+age.norm+hrs.norm+sex+race+marital.status+occupation+workclass,
                 data= imput.train,
                 method= 'class')   
rpart.plot(nat.cart)      #Not Very usefull but interesting. Race=Asian from Asia
summary(nat.cart)

#KNN
  #work KNN
census.complete.full<-census.complete.full[sample(1:nrow(census.complete.full), length(1:nrow(census.complete.full))), 1:ncol(census.complete.full)]
work.values<- census.complete.full[,c(17,21:28,36:47)]
work.targets<-census.complete.full[,2]
work.knn<- splitForTrainingAndTest(work.values,work.targets, ratio = .1)
work.knn.model<-knn(work.knn$inputsTrain,work.knn$inputsTest, cl=work.knn$targetsTrain,k=10)
work.matrix<-table(work.knn$targetsTest,work.knn.model)
sum(work.matrix[1,1],work.matrix[2,2],work.matrix[3,3])/1502

work.knn.optimal<-c(rep(0,28))
for(i in 1:28){
  work.knn.model<-knn(work.knn$inputsTrain,work.knn$inputsTest, cl=work.knn$targetsTrain,k=i)
  work.knn.optimal[i]<-100*sum(work.knn$targetsTest == work.knn.model)/1502
}
str(work.knn.optimal)
qplot(seq_along(work.knn.optimal), work.knn.optimal) #we can see that the acuraccy of a non-wieghted plot
  #occupation.KNN
occ.values<-census.complete.full[,c(17,21:28,36:47)]
occ.target<-census.complete.full[,7]
occ.knn<- splitForTrainingAndTest(occ.values,occ.target, ratio = .1)
occ.knn.model<-knn(occ.knn$inputsTrain,occ.knn$inputsTest, cl=occ.knn$targetsTrain,k=5)
table(occ.knn$targetsTest, occ.knn.model)

occ.knn.optimal<-c(rep(0,28))
for(i in 1:28){
  occ.knn.model<-knn(occ.knn$inputsTrain,occ.knn$inputsTest, cl=occ.knn$targetsTrain,k=i)
  occ.knn.optimal[i]<-100*sum(occ.knn$targetsTest == occ.knn.model)/1502
}
occ.knn.optimal  #as we can see un-wieghted knn is not effective at predicting our missing variables

  #Nation KNN
nat.values<- census.complete.full[,c(17:28,31:40)]
nat.target<-census.complete.full[,14]
nat.knn<- splitForTrainingAndTest(nat.values,nat.target, ratio = .1)
nat.knn.model<-knn(nat.knn$inputsTrain,nat.knn$inputsTest, cl=nat.knn$targetsTrain,k=5)
table(nat.knn$targetsTest, nat.knn.model)

nat.knn.optimal<-c(rep(0,28))
for(i in 1:28){
  nat.knn.model<-knn(nat.knn$inputsTrain,nat.knn$inputsTest, cl=nat.knn$targetsTrain,k=i)
  nat.knn.optimal[i]<-100*sum(nat.knn$targetsTest == nat.knn.model)/1502
}
nat.knn.optimal #8% error not to bad. I feel like natitive country will be the most accurate to impute.
