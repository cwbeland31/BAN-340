#Running the models (note this is with out the missing data still need to take care of that first)
census.final<-census.complete.full[sample(1:nrow(census.complete.full), length(1:nrow(census.complete.full))), 1:ncol(census.complete.full)]
set.seed(876521)
#knn
names(census.final)
census.final.values<- census.final[,c(17:40,42:47)]
census.final.targets<-census.final[48]
census.final<- splitForTrainingAndTest(census.final.values, census.final.targets, ratio=.1)
census.final.knn.train<- knn.cv(census.final$inputsTrain, cl= census.final$targetsTrain, k=3)
table(census.final.knn.train, census.final$targetsTrain)
sum(census.final.knn.train== census.final$targetsTrain)/13509
#based of the training tested by the training we can predict at 81%
census.final.knn.accuracy<-c(rep(0,20))
for(i in 1:20){
  census.final.knn<- knn(census.final$inputsTrain, census.final$inputsTest, cl= census.final$targetsTrain,k=i)
 census.final.knn.accuracy[i]<-100*sum(census.final$targetsTest == census.final.knn)/1502
}
census.final.knn.accuracy
#We can predict wether someone makes over 50k with about 83% accuracy according to our test set

#Decision trees
