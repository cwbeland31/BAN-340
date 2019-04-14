#library
library('tidyverse')
library("rpart")
library("rpart.plot")
library("C50")
library('class')
library('merTools')
library('mice')
library('naniar')
library('nnet')
#Adult Dataset
census<- read.csv('Adult.txt', header= FALSE)
census<- setNames(census,c('age','workclass','fnlwgt','education','education.num','marital.status','occupation','relationship','race','sex','capital.gain','capital.loss','hours.per.week', 'native.country', 'income.class'))

#looking at missing values
census.copy<-census
missing.values.workclass<-census.copy%>%filter(workclass==' ')
missing.values.occupation<-census.copy%>%filter(occupation==' ')
missing.values.native<-census.copy%>%filter(native.country==' ')
missing.values.complete<-rbind(missing.values.native,missing.values.occupation,missing.values.workclass)
dup<-duplicated(missing.values.complete[,3])
missing.values.complete<-unique.data.frame(missing.values.complete)
sum(census.copy$workclass==' ')/length(census$workclass)*100  #5% of workclass is missing
sum(census.copy$occupation== ' ')/length(census$occupation)*100 #5% of occupation is missing
sum(census.copy$native.country==' ')/length(census$occupation)*100 #1.79% of native country is missing
census.copy.na<- census.copy%>% replace_with_na(replace = list(workclass=" "))
census.copy.na<- census.copy.na%>% replace_with_na(replace= list(occupation=' '))
census.copy.na<- census.copy.na%>% replace_with_na(replace= list(native.country=' '))
str(census.copy.na)
census.copy.na$native.country
md.pattern(census.copy.na[,c(2,7,14)])
census.copy.na[which(is.na(census.copy.na$workclass)),]
table(missing.values.complete$workclass)
table(missing.values.complete$sex)
table(missing.values.complete$relationship)
table(missing.values.complete$native.country)
rm(census.copy,census.copy.na,dup,missing.values.native,missing.values.occupation, missing.values.workclass)
#nothing seems to be out of the ordinary
#removing missing values and adding 'missing'
levels(census$workclass)[1]= "?"
levels(census$occupation)[1]='?'
levels(census$native.country)[1]= '?'
census<- census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')
#duplicates
summary(census)
str(census)
dup<-duplicated(census)
which(dup== TRUE)
census<-census[-c( 4469,  4682,  8471, 10747, 12106, 13949, 15780, 17330, 19754, 19911, 20264, 20664, 20727, 20845, 23973, 24384, 26164, 26435,
                  26741, 27022, 28580, 29641, 30021),]
#Function
scew<-function(x){
  (3*(mean(x)-median(x)))/sd(x)
}
twoTtest<- function(x,y){
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

#making a copy of census before commencing EDA changes.
census.b<-census

#---------------------------------------#
#----Data understanding Phase EDA-------#
#---------------------------------------# 
 #---reassinging Levels------------#
    #workclass
table(census$workclass)
table(census$workclass, census$income.class)
levels(census$workclass)<- c("?","Gov","Gov", "?", "Private", "Self Employed", "Self Employed", "Gov", "?") #note, I got ride of unemployed here.
    #education
table(census$education) 
table(census$education, census$income.class)
levels(census$education)<- c("No HS Degree","No HS Degree","No HS Degree","No HS Degree",
                             "No HS Degree","No HS Degree","No HS Degree","HS Grad","HS Grad",
                             "Undergrad","Doctoral","HS Grad","Graduate","No HS Degree","Prof-school",
                             "HS Grad")
census$education<-factor(census$education, levels=c("No HS Degree","HS Grad", "Undergrad", "Graduate", "Prof-school", 'Doctoral'))
  #marital.status
table(census$marital.status)
levels(census$marital.status)<- c("Divorced", "Married", "Married", "Married","Not-Married", "Not-Married", "Widowed")
  #relationship
table(census$relationship)
levels(census$relationship)<- c('Married','Not-in-family','Other-relative','Own-child', 'Not-Married', 'Married')
  #occupation
table(census$occupation)
ot1<- table(census$income.class,census$occupation)
ot2<- ot1
ot2[1,] <- (ot2[1,]/margin.table(ot1,2))*100
ot2[2,]<- (ot2[2,]/margin.table(ot1,2))*100
ot2 #percentages 
table(census$occupation, census$income.class) #I tried to base these levels off of the income level, size, and simularity
levels(census$occupation)<- c('?', 'Admin','?', 'skilled.labor', 'white.collar','labor','labor', 
                              'service', 'labor', 'labor','white.collar', 'service','sales', 
                              'Tech-support','skilled.labor') 
  #race
table(census$race) #basic rename
levels(census$race)<- c("Indian", "Asian", "Black", "Other", "White")
  #native.country
table(census$native.country)
levels(census$native.country)<-c('?','Asia','NorthAmerica',
  'Asia','SouthAmerica','CentralAmerica',
  'CentralAmerica','SouthAmerica','CentralAmerica',
  'Europe','Europe','Europe',
  'Europe', 'CentralAmerica', 'CentralAmerica',
  'Europe', 'CentralAmerica','Asia',
  'Europe','Asia','Asia',
  'Europe','Europe','CentralAmerica',
  'Asia','Asia','CentralAmerica',
  'CentralAmerica','CentralAmerica','SouthAmerica',
  'Asia','Europe','Europe',
  'CentralAmerica','Europe','?',
  'Asia','Asia', 'SouthAmerica',
  'NorthAmerica','Asia','Europe')
table(census$native.country)
str(census)

#Removing missings generated in re-leveling
levels(census$income.class)<- c("<=50K",">50K")
census<- census%>%filter(workclass!='?')%>%filter(occupation!='?')%>%filter(native.country!='?')

#multi-coliniarity
cor(census[,c(1,3,5,11,12,13)]) #all numerics are are not correlated.

#income.class
table(census$income.class)
7491/30045
#about 25% of people make over 50k thus model's accuracy should be based on a minumum of 75% accuracy

#Age
ggplot(census, aes(y=age))+
  geom_boxplot()

boxplot.stats(census$age)

ggplot(census,aes(x=age,fill=income.class))+
  geom_density(stat = "count", colour='black')+
  theme_minimal()+
  ggtitle( 'Age frequency compared to income'
  )
  

ggplot(census,aes(x=age, fill=income.class))+
  geom_histogram(position= "fill", bins = 60)+
  theme_minimal()+
ggtitle('Normalized comparison of age with respect to income')
age.na<-is.na(census$age)
which(age.na=="TRUE") #No missing data

age.out<-census[which(census$age>78),]
age.out #Nothing looks out of the ordinary, #highest age is 90. Did the cenesus stop at 90?, a decent amount of outliers. THey are logicals tho.
census%>%filter(age==90)
#age is right scewed, look to unscew
#This shows us that the majority of people that make over 50k are in their peak work years. 
#Intuitevly, this makes sense because older people have more experience thus better job prospects
qqnorm(census$age, col= "blue") #confirmation of right scewed, this is expected in age, It does not seem agregious and is good to use for prediction
qqline(census$age, col="red")

#bining age in even width for comparison purposes
age.labs <- c(paste(seq(0, 80, by = 20), seq(0 + 20 - 1, 100 - 1, by = 20),
                    sep = "-"), paste(100, "+", sep = ""))
census$age.e.bin <- cut(census$age, breaks = c(seq(0, 100, by = 20), Inf), labels = age.labs, right = FALSE)

#Work class
ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "stack", colour= 'black')+
  theme_minimal()+
  ggtitle('Frequencies of Workclass')#alternate stack and fill to look at count and normalized graphs
ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "fill",colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
  labs( y= 'ratio'
  )

#These two graphs give us an idea of how the workclass of a person might effect his/hers earning prospects
#It seems that the majority of the data fall in the Private sector but those who work in government and self have better 
#have a better percentage of earning over 50k. However, I doubt this has predictive capabilities as the private class
#is so large that it probably includes many lowerpaying jobs as well as more higher paying jobs.
workclass.table<- table(census$workclass,census$income.class)
work.inc.margin<- round(prop.table(workclass.table, margin = 1),2)*100
workclass.table
work.inc.margin
#This table shows the actual percentages of each specific workclass's  
ggplot(census, aes(x=age.e.bin, fill=workclass))+
  geom_bar(position = 'fill')
ggplot(census, aes(x=workclass, fill=sex))+
  geom_bar(position = 'fill')
#here we can see the ratio of males and females in each respective work class. Nothing seems out of the ordinary
#and looks like the ratios are about what you would expect for each workclass based on the normal females

#fnlwgt means the # of people this observation represents. Probably not relevent for analysis
ggplot(census, aes(x= fnlwgt, fill= income.class))+
  geom_histogram(position = "fill")

#Education
ggplot(census, aes(x= education))+
  geom_bar(fill="lightblue")
ggplot(census, aes(x= education, fill=income.class))+
  geom_bar(position="stack")
ggplot(census, aes(x= education, fill= sex))+
  geom_bar(position= "stack")
ggplot(census, aes(x=education, fill= workclass))+
  geom_bar(position= "fill")
                      #note: due to the re-leveling edu.num is none-representative of level.
                            #furthermore it is best to level indicators at 0-1 as the difference of a No-Hs and HS and undergrad-grad is subjective
#Marital.status
ggplot(census, aes(x= marital.status,fill= income.class))+
  geom_bar(position = "fill")
ggplot(census, aes(x= marital.status,fill= income.class))+
  geom_bar(position = "stack")
census%>% filter(age<20)%>%filter(marital.status== "Married")

#Relationship status
ggplot(census, aes(x= relationship, fill= income.class))+   
  geom_bar(position = "stack")
ggplot(census, aes(x= relationship, fill= income.class))+   
  geom_bar(position = "fill")

ggplot(census, aes(x=relationship, fill= marital.status))+
  geom_bar(position= "fill")

ggplot(census, aes(x=relationship, fill= sex))+
  geom_bar(position= "stack")
#trying to combine information in marital.status and relationship
#It will probably best to leave out one of relationship or marital status. We will keep relationship
#because decision trees found it to have more predictive power. 

#capital income
ggplot(census, aes(capital.gain, fill= income.class))+
  geom_histogram(position= 'stack')
ggplot(census, aes(capital.loss, fill= income.class))+
  geom_histogram(position= 'stack')
boxplot(census$capital.gain)
boxplot(census$capital.loss)
#looking at numerics/ generalize
census$capital.gain.yes<- census$capital.gain>0
census$capital.loss.yes<- census$capital.loss>0
cap.gain<-cap.loss<- c(rep(0, length(census$age)))
for(i in 1:length(census$age)){
  if(census$capital.gain.yes[i]== "TRUE")cap.gain[i]<-1
  if(census$capital.loss.yes[i]== "TRUE")cap.loss[i]<-1
}
census$capital.gain.yes<- cap.gain   #extremly skewed but might be interesting to see this data used in decision trees.
census$capital.loss.yes<-cap.loss 

table(census$capital.gain.yes) #only 2538 people had capital gains
table(census$capital.loss.yes) #only 1427 people posted capital losses. 
#both these data sets are right skewed, probably not suited for regression.
qqnorm(census$capital.gain, col= "blue") 
qqline(census$capital.gain, col="red")
qqnorm(census$capital.loss, col= "blue") 
qqline(census$capital.loss, col="red") 
#lots of 99999 capital gains. Are they a data miss entry? all have above 50k. Does the system max out at 99999 capital gains
summary(census$capital.loss)
#max loss was 3770.0 seems okay. No red flags.
prop.table(table(census$income.class,census$capital.gain.yes),2) #62% of those with>50k  have capital gain
summary(census$income.class)
prop.table(table(census$income.class,census$capital.loss.yes),2) #About a 50-50 split of those with >50k have capital losses, not very predictivr

capital.gain.or.loss <- (census$capital.gain>0|census$capital.loss>0) #seem more likly that a person who makes over 50k has money in the market.
prop.table(table(census$income.class, capital.gain.or.loss),2)

ggplot(census, aes(x= capital.gain.or.loss, fill= income.class))+
  geom_bar(position= "fill")+
theme_minimal()
#occupation
ggplot(census, aes( x=occupation, fill= income.class))+ 
  geom_bar(position = "fill")
table(census$income.class, census$occupation)
table(census$occupation)
ggplot(census, aes( x=occupation, fill= income.class))+
  geom_bar(position = "dodge")
ggplot(census, aes( x=occupation, fill= income.class))+
  geom_bar(position = "fill")

#Gender
levels(census$sex)<-c('Female','Male') #removing space
table(census$income.class, census$sex)
gt1<- table(census$income.class,census$sex)
gt2<- gt1
gt2[1,] <- (gt2[1,]/margin.table(gt1,2))*100
gt2[2,]<- (gt2[2,]/margin.table(gt1,2))*100
gt2

ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="fill")

ggplot(census, aes(race, fill= sex))+
  geom_bar(position="fill")

ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="stack") #much fewer females than males. Possible bias: calculated differently from the standard pouplation 
levels(census$income.class)
census%>% filter(race== "White")%>% filter(sex=="Male")%>% count()
census%>% filter(race== "White")%>% filter(sex=="Male")%>% filter(income.class== ">50K")%>%count()
5865/18011

#Race
race.table<-table(census$income.class, census$race)
race.table #majority are white
margin.table(race.table,2)
rt1<-table(census$income.class, census$race)
rt2<- rt1
rt2[1,] <- (rt2[1,]/margin.table(rt1,2))*100
rt2[2,]<- (rt2[2,]/margin.table(rt1,2))*100
rt2 #race percentages, asians and whites are more likly to make over 50k
ggplot(census, aes(race, fill= workclass))+
  geom_bar(position = 'fill')
ggplot(census, aes(race, fill= income.class))+
  geom_bar(position = "fill")
ggplot(census, aes(education, fill= race))+
  geom_bar(position = 'fill')
levels(census$race)
#heavily scewed toward white which is expected, also white is pretty high

#hours per week 
boxplot(census$hours.per.week)
boxplot.stats(census$hours.per.week)
max(census$hours.per.week)
hrsperweek.out<-census[which(census$hours.per.week>52|census$hours.per.week<33),]
summary(hrsperweek.out)
census[which(census$hours.per.week== 99),]
census%>%filter(income.class==" >50K.")%>%filter(hours.per.week== 99)

qqnorm(census$hours.per.week, col= "blue") 
qqline(census$hours.per.week, col="red") #closser to normality than capital.gains/loss

ggplot(census, aes(x= hours.per.week, fill= income.class))+
  geom_histogram(position = "stack") #almost half work 40hrs a week
census$hours.per.week<- as.numeric(census$hours.per.week)
hours.per.week.bin<- cut(census$hours.per.week, breaks = c(0,39, Inf), labels = c("<40hrs", ">=40hrs"))
over.40hrs<- c(rep(0,length(census$age)))
for(i in 1:length(1:length(census$age))){
  if(hours.per.week.bin[i]== ">=40hrs")over.40hrs[i]<-1
}
census$hours.per.week.over40<-over.40hrs
ggplot(census, aes(x= hours.per.week.bin, fill= income.class))+
  geom_bar(position = "stack") #This predictor could help as those who work less than 40hrs do not make over 50k often


census%>% filter(hours.per.week.bin== "<40hrs")%>% filter(sex== "Male")%>% count()
census%>% filter(hours.per.week.bin== "<40hrs")%>% filter(sex== "Female")%>% count()
table(census$sex)
3551/9743 #36% of females work less than 40hrs per week
3136/20302 #15% of males work less than 40hrs per week 
ggplot(census, aes(age.e.bin, fill= hours.per.week.bin))+ 
  geom_bar(position = "fill")
#The data does not include people who do not work 0 hours. However, it does include unemployed people, Is it assumed the hours they work are froma  previous job?
#Also it is not normally distributed as with the rest of the numbers so linear regression probably won't work.

#Native Country
ggplot(census, aes(x=native.country))+
  geom_bar()
ggplot(census, aes(x=native.country, fill= income.class))+
  geom_bar(position = "fill")
ggplot(census, aes(x=native.country, fill= sex))+
  geom_bar(position= "fill")
ggplot(census, aes(x=native.country, fill= workclass))+
  geom_bar(position= "fill")

#Nationality is heavily scewed toward Americans Also looks pretty even in for predictability when normalized.
#might try it in models but most likly will remove.

#---------------------#
#----Pre-process------#
#---------------------#

#Missing data. Due to the size of this data set and the missingness it was decided to omit the missing data.
#However, imputation methods where applied and primarily unsucessful. If data was to be retained it would only be 
#a mininmal as occupation was not able to obtain a satisfactory result. A seperate R file will be included for
#that attempt.

#removing added variables, and removing unused factor levels
census.full<-census
rm(hrsperweek.out,age.out)
census$age.e.bin<-NULL
census$education.num<-NULL
census$fnlwgt<-NULL
census$marital.status #due to the simularities that relationship captures
census$capital.gain<-NULL
census$capital.loss<-NULL
census$hours.per.week<-NULL
census$native.country
census$workclass<-factor(census$workclass)
census$occupation<-factor(census$occupation)
census$native.country<-factor(census$native.country)
summary(census)


#adding dummy variables (1-k)
income.class.over50k<- c(rep(0,length(census$age)))
for(i in 1:length(census$age)){
  if(census$income.class[i]==">50K")income.class.over50k[i]<-1
}
census$income.class.ind<-income.class.over50k

census.dummy<-model.matrix(income.class~., data= census)
census.dummy<- as.data.frame(census.dummy)

#spliting train and test
set.seed(15748)
samp<-sample(30045,6000) #having a large datset allows for a larger split 80:20
census.train<-census[-samp,]
census.test<-census[samp,]
census.train.dummy<- census.dummy[-samp,]
census.test.dummy<- census.dummy[samp,]
census.train.dummy$`(Intercept)`<-NULL
census.test.dummy$`(Intercept)`<-NULL

census.train.dummy$age<- minmax.nrom(census.train.dummy$age)
census.test.dummy$age<- minmax.nrom(census.test.dummy$age)
summary(census.train.dummy$age)

#checking for good splits. note that dummy splits are the same as the non-dummy splits thus no need to check
#Age T-test
census.train$age<- as.numeric(census.train$age)
census.test$age<- as.numeric(census.test$age)
age.tvalue<-twoTtest(census.test$age,census.train$age)
age.df<-min.df(census.train$age,census.test$age)
age.pvalue<-twotailpvalue(age.tvalue,age.df)
age.pvalue

#Income.class chi-square
table(census.train$income.class)
table(census.test$income.class)
inc.mat<- matrix(c(18062,4492,5983,1508), nrow = 2)
chisq.test(inc.mat)

#education
table(census.train$education)
table(census.test$education)
edu.mat<- matrix(c(3049,682,14956,3806,4040,979,1276,342,427,114,297,77), nrow = 2)
chisq.test(edu.mat)

#normalize
census.train$age<- minmax.nrom(census.train$age)
census.test$age<- minmax.nrom(census.test$age)


#----------------------------------#
#-----Modeling Phase---------------#
#----------------------------------#
inc.glm<- glm(income.class.ind~.,data = census.train.dummy, family = 'binomial')
summary(inc.glm)
#here we can get a good feel for the predicatbility of each variable. Specifically looking at marital status we
#we cab see marital status has a strong effect along with relationship. However married and unmarried is already
#established in relationship. Also Relationship's aditional variabls hold more signficance as apposed to married
#Thus sence their is redundency in marital.status and relationship hold more useful predictor variables 
#marital status will be removed. We can also see that race, workclass, and native.country play a small role as well.
#The reason for race and native.country play a small role. This is due the the high proportion of white north American

census.train$marital.status<-NULL
census.test$marital.status<-NULL
census.train.dummy[,c(9:11)]<-NULL
census.test.dummy[,c(9:11)]<-NULL

inc.glm<- glm(income.class.ind~.,data = census.train.dummy, family = 'binomial')
summary(inc.glm)
anova(inc.glm, test = 'Chisq')
inc.glm.predict<- predict(inc.glm,newdata = census.test.dummy, type = 'response' )
inc.glm.predict <- ifelse(inc.glm.predict> 0.5,1,0)
table(inc.glm.predict,census.test$income.class.ind)
(334+634)/6000 #16% error

#Removing Race and Native Country due to non-significance
census.train.1<- census.train
census.test.1<- census.test
census.train.dummy.1<- census.train.dummy
census.test.dummy.1<- census.test.dummy

census.train.1$native.country<-NULL
census.test.1$native.country<-NULL
census.train.1$race<-NULL
census.test.1$race<-NULL

census.train.dummy.1[, c(19:22,24:28)]<-NULL
census.test.dummy.1[,c(19:22,24:28)]<-NULL

inc.1.glm<- glm(income.class.ind~.,data = census.train.dummy.1, family = 'binomial')
summary(inc.1.glm)
anova(inc.1.glm, test = 'Chisq')
inc.glm.predict.1<- predict(inc.1.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.1 <- ifelse(inc.glm.predict.1> 0.5,1,0)
table(inc.glm.predict.1,census.test$income.class.ind)
(332+645)/6000 #We can see that these variables had very little effect in the predictability
#Now lets look at Workclass which seems to have very low predictive power based off of the
#logistics model and will be removed and tested to see reduction in accuracy

census.train.1$workclass<-NULL
census.test.1$workclass<-NULL
census.train.dummy.1$workclassPrivate<-NULL
census.train.dummy.1$`workclassSelf Employed`<-NULL
census.test.dummy.1$workclassPrivate<-NULL
census.test.dummy.1$`workclassSelf Employed`<-NULL

inc.2.glm<- glm(income.class.ind~.,data = census.train.dummy.1, family = 'binomial')
summary(inc.2.glm)
anova(inc.2.glm, test = 'Chisq')
inc.glm.predict.2<- predict(inc.2.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.2 <- ifelse(inc.glm.predict.2> 0.5,1,0)
table(inc.glm.predict.2,census.test$income.class.ind)
(331+643)/6000 #16% missclass rate
#Though all these are pretty close but the best model is the one with race,native.country, and workclass removed
#looking back at the EDA the graphs seem to back up the models assumptions. Also note that it does look like some of
#these countries/ races do seem to have a higher percentage in the lower income but due to the balance these variables 
#do not seem to affect prediction. However we will put them back into some of the modeles and compare

#----decision trees-------#
  #----C50------#
x<- census.train[,c(1:9,11:13)]
y<-census.train[,10]
income.c50<- C5.0(x,y)
summary(income.c50) #error of (15.1%)
income.c50.predict<- predict.C5.0(income.c50, census.test[,c(1:9,11:13)])
table(income.c50.predict,census.test$income.class)
(359+631)/6000   #error of  (16.5%) maybe a little overfitting going on but pretty even
  #---C50--Without native,race, and workclass---#
x<- census.train.1[,c(1:5,7:9)]
y<-census.train.1[,6]
income.c50<- C5.0(x,y)
summary(income.c50) #error of (15.7%)
income.c50.predict<- predict.C5.0(income.c50, census.test.1[,c(1:5,7:9)])
table(income.c50.predict,census.test$income.class)
(388+600)/6000 #error of 16.4% 
#As the results show the three variables removed had little to no effect on the efficieny of our model and 
#actually improved the accuracy by .01% also it is important to strive for simplicity when possible

  #----CART----#
x<-census.train[,-14]
income.rpart<- rpart(income.class~., data= x)
summary(income.rpart)
rpart.plot(income.rpart)
income.rpart.predict<- predict(income.rpart, census.test, type= 'class')
table(census.test$income.class, income.rpart.predict)
(913+226)/6000 #18% error
  #note how CART does only uses relationship+ education thus no point in re-running without workclass, ect...
  #----KNN-----#
x<-census.train.dummy[,-32]
y<-census.test.dummy[,-32]
income.knn.optimal<-c(rep(0,30))
for(i in 1:30){
  income.knn.model<-knn(x,y, cl=census.train$income.class,k=i)
  income.knn.optimal[i]<-100*sum(census.test$income.class == income.knn.model)/6000
}
income.knn.optimal #about 82.55 accuracy at k=22, however what about tied??
#-----KNN-without work, race, native------#
x<-census.train.dummy.1[,-21]
y<-census.test.dummy.1[,-21]
income.knn.optimal.1<-c(rep(0,30))
for(i in 1:30){
  income.knn.model.1<-knn(x,y, cl=census.train$income.class,k=i)
  income.knn.optimal.1[i]<-100*sum(census.test$income.class == income.knn.model)/6000
}
max(income.knn.optimal.1) #literaly wores, I wonder if I could stretch the axisis
#---Neural Networks----#
x<-census.train.dummy[,-32]
x$income.class<- census.train$income.class
income.nnet<- nnet(income.class~., data = x, size= 10, maxit= 1000)
income.nn.predict<- predict(income.nnet, census.test.dummy, type= 'class')
table(income.nn.predict, census.test$income.class)
(595+404)/6000 #16.65
#---Neural Networks--without work, race, and native-------#
x<-census.train.dummy.1[-21]
x$income.class<- census.train$income.class
income.nnet.1<- nnet(income.class~., data = x, size= 10, maxit= 1000, decay= .01)
income.nn.predict.1<- predict(income.nnet.1, census.test.dummy.1, type= 'class')
table(income.nn.predict.1, census.test$income.class)
#
(359+600)/6000 #15.98
#--------------------#
#--base line models--#
#--------------------#

#adding dummy variables
census.b.indicator<-model.matrix(income.class~., data= census.b)
census.b.indicator<- as.data.frame(census.b.indicator)
#Lets go into predicting without editing anything
set.seed(14632)
samp<-sample(30139,6000)
census.train.b<-census[-samp,]
census.test.b<-census[samp,]
census.train.b.ind<- census.b.indicator[-samp,]
census.test.b.ind<- census.b.indicator[samp,]
#Need to go back a make sure the test is representative of the sample
  #decsison trees, C.50
x<- census.train.b[,c(1:14)]
y<- census.train.b[, 15]
income.c50<- C5.0(x,y)
summary(income.c50)
income.c50.predict<- predict.C5.0(income.c50, census.test.b[,1:14])
table(census.test.b$income.class, income.c50.predict)
(2125+491)/3000 #87% 
#CART
census.train.b<-census.train.b[,-14]
income.rpart<- rpart(income.class~., data= census.train.b)
summary(income.rpart)
rpart.plot(income.rpart)
income.rpart.predict<- predict(income.rpart, census.test.b, type= 'class')
table(census.test.b$income.class, income.rpart.predict)
(901+240)/6000 #85%
  #KNN
x<- census.train.b.ind[,16:101]
y<- census.test.b.ind[,16:101]
z<- census.train.b[,15]
income.knn<- knn(x,y,cl= z, k=15)
table(census.test.b$income.class, income.knn)
(2093+412)/3000 #about 86% accuracy
  #Neural Network
x$income.class<- census.train.b[,15]
income.nn<- nnet(income.class~.,data= x, size=10, maxit= 500)
income.nn.predict<- predict(income.nn, census.test.b.ind, type= 'class')
table(income.nn.predict, census.test.b$income.class)
(2079+462)/3000 #about 85% accuracy
