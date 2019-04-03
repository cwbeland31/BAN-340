#Base line percentages
census.compare<- read.csv('Provided dataset(1).csv', header = TRUE)

#normalize, remove dubs, and cleaning
minmax.nrom<- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
summary(census.compare)
asdfsdaf<-duplicated(census.compare)
which(asdfsdaf== "TRUE")
census.compare<-census.compare[-c(865, 11190,11213,13849,15961),]
rm(asdfsdaf)
set.seed(876521)
census.compare<-census.compare[sample(1:nrow(census.compare), length(1:nrow(census.compare))), 1:ncol(census.compare)]
census.compare<-splitForTrainingAndTest(census.compare[,-15],census.compare[,15], ratio = .1)
census.compare.imput.train<-as.data.frame(census.compare$inputsTrain)
  census.compare.imput.train$age<- as.numeric(census.compare.imput.train$age)
  attr(census.compare.imput.train$workclass, "ATT") <- NULL
  attr(census.compare.imput.train$fnlwgt, "ATT") <- NULL
  attr(census.compare.imput.train$education, "ATT") <- NULL
  census.compare.imput.train$education.num<- as.numeric(census.compare.imput.train$education.num)
  attr(census.compare.imput.train$marital.status, "ATT") <- NULL
  attr(census.compare.imput.train$occupation, "ATT") <- NULL
  attr(census.compare.imput.train$relationship, "ATT") <- NULL
  attr(census.compare.imput.train$race, "ATT") <- NULL
  attr(census.compare.imput.train$sex, "ATT") <- NULL
  census.compare.imput.train$capital.gain<- as.numeric(census.compare.imput.train$capital.gain)
  census.compare.imput.train$capital.loss<- as.numeric(census.compare.imput.train$capital.loss)
  census.compare.imput.train$hours.per.week<- as.numeric(census.compare.imput.train$hours.per.week)
  attr(census.compare.imput.train$native.country, "ATT") <- NULL
census.compare.imput.test<- as.data.frame(census.compare$inputsTest)
  census.compare.imput.test$age<- as.numeric(census.compare.imput.test$age)
  attr(census.compare.imput.test$workclass, "ATT") <- NULL
  attr(census.compare.imput.test$fnlwgt, "ATT") <- NULL
  attr(census.compare.imput.test$education, "ATT") <- NULL
  census.compare.imput.test$education.num<- as.numeric(census.compare.imput.test$education.num)
  attr(census.compare.imput.test$marital.status, "ATT") <- NULL
  attr(census.compare.imput.test$occupation, "ATT") <- NULL
  attr(census.compare.imput.test$relationship, "ATT") <- NULL
  attr(census.compare.imput.test$race, "ATT") <- NULL
  attr(census.compare.imput.test$sex, "ATT") <- NULL
  census.compare.imput.test$capital.gain<- as.numeric(census.compare.imput.test$capital.gain)
  census.compare.imput.test$capital.loss<- as.numeric(census.compare.imput.test$capital.loss)
  census.compare.imput.test$hours.per.week<- as.numeric(census.compare.imput.test$hours.per.week)
  attr(census.compare.imput.test$native.country, "ATT") <- NULL
  census.compare.imput.train[,c(1,5,11,12,13)]<- lapply(census.compare.imput.train[,c(1,5,11,12,13)], minmax.nrom)
  census.compare.imput.test[,c(1,5,11,12,13)]<- lapply(census.compare.imput.test[,c(1,5,11,12,13)], minmax.nrom)
  summary(census.compare.imput.test)
  summary(census.compare.imput.train)
#decison tree
census.compare.imput.train$income.class<- as.factor(census.compare$targetsTrain) #because Rpart is wacky
cart.base<- rpart(income.class~ age+workclass+education.num+marital.status+occupation+relationship+race+sex+capital.gain+capital.loss+hours.per.week+native.country, data = census.compare.imput.train, method = 'class')
cart.base
rpart.plot(cart.base)
x<-rpart.predict(cart.base, census.compare.imput.test, type = 'class')
table(x,census.compare$targetsTest)
(1196+201)/1628
#can predict with 85% accuacy doin nothing 
census.compare.imput.train<-census.compare.imput.train[,-3]
census.compare.imput.test<-census.compare.imput.test[,-3]
baseC50<- C5.0(census.compare.imput.train[,-14], census.compare.imput.train[,14])
summary(baseC50)
baseC50predict<-predict.C5.0(baseC50, census.compare.imput.test)
table(baseC50predict,census.compare$targetsTest)
(1192+222)/1628 #about 86% error as well.
#KNN
knn.train.values.base<- census.compare.imput.train[,c(1,10:12)]
knn.test.values.base<- census.compare.imput.test[,c(1,10:12)]
base.knn.modeld<- knn(knn.train.values.base,knn.test.values.base, cl= census.compare$targetsTrain, k=21)
table(base.knn.modeld, census.compare$targetsTest)
(1161+144)/1628
#roughly 80% accuracy on average with k=21

#Neural Network


