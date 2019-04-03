census<- read.csv('Provided dataset(1).csv', header = TRUE)
library('tidyverse')
library('scales')
library("rpart")
library("rpart.plot")
library("C50")
library('class')
library('mice')
library('naniar')
library('neuralnet')
library('RSNNS')
summary(census)
str(census)
dup<-duplicated(census)
which(dup== "TRUE")
census<-census[-c(865, 11190, 11213, 13849, 15011, 15230, 15961),] #removing duplicated rows
str(census)
  #Focus on catagorial data mining methods and prep
  #Goal is to predict if income is greater than 50k

#Function
scew<-function(x){
(3*(mean(x)-median(x)))/sd(x)
}

#re-asigning levels (this is kinda messy but I feel like some of these need to be condenced for generalization, also need to introduce <NA> for mice)
levels(census$workclass)<- c("?","Gov","Gov", "?", "Private", "Self Employed", "Self Employed", "Gov", "?") #note, I got ride of unemployed here.
levels(census$education)<- c("No HS Degree","No HS Degree","No HS Degree","No HS Degree",
                             "No HS Degree","No HS Degree","No HS Degree","HS Grad","HS Grad",
                             "Undergrad","Doctoral","HS Grad","Graduate","No HS Degree","Prof-school",
                             "HS Grad")
levels(census$marital.status)<- c("Divorced", "Married", "Married", "Married","Not-Married", "Not-Married", "Widowed")
levels(census$occupation)<- c('?', 'office','?', 'labor', 'white-collar', 
                              'labor', 'labor', 'service', 'service', 'service',
                              'white-collar', 'service', 'sales', 'office','labor') 
levels(census$sex)<-c('Female','Male')
levels(census$race)<- c("Indian", "Asian", "Black", "Other", "White")
levels(census$native.country)<- c("?", "Asia", "Central America", "Asia","South America", "Carabian", 
                                  "Carabian", "South America", "Central America", "Europe", "Europe", "Europe", 
                                  "Europe", "Central America", "Carabian","Central America", "Asia", "Europe", 
                                  "Asia", "Asia", "Europe", "Europe", "Carabian", "Asia", 
                                  "Asia", "Central America", "Central America", "Carabian", "South America", "Asia", 
                                  "Europe", "Europe", "Carabian", "Europe", "?", "Asia",
                                  "Asia", "Carabian", "North America", "Asia", "Europe")
levels(census$income.class)<- c("<=50K",">50K")

#MCAR,MAR, or MNAR? 'lets see if we can figuter out the ?
census.copy<-census
missing.values.workclass<-census.copy%>%filter(workclass=='?')
missing.values.occupation<-census.copy%>%filter(occupation=='?')
missing.values.native<-census.copy%>%filter(native.country=='?')
missing.values.complete<-rbind(missing.values.native,missing.values.occupation,missing.values.workclass)
dup<-duplicated(missing.values.complete[,3])
missing.values.complete<-unique.data.frame(missing.values.complete)
sum(census.copy$workclass=='?')/length(census$workclass)*100  #6% of workclass is missing
sum(census.copy$occupation== '?')/length(census$occupation)*100 #6% of occupation is missing
sum(census.copy$native.country=='?')/length(census$occupation)*100 #1.9% of native country is missing
census.copy.na<- census.copy%>% replace_with_na(replace = list(workclass="?"))
census.copy.na<- census.copy.na%>% replace_with_na(replace= list(occupation='?'))
census.copy.na<- census.copy.na%>% replace_with_na(replace= list(native.country='?'))
str(census.copy.na)
census.copy.na$native.country
md.pattern(census.copy.na[,c(2,7,14)])
census[which(census$native.country=='?'),1:15]
census.copy.na[which(is.na(census.copy.na$workclass)),] #looking at these stats nothing stands out seems pretty proportionate of data
rm(census.copy,missing.values.complete,missing.values.native, missing.values.occupation, missing.values.workclass,dup) #trying to keep the enviorment tidy
levels(census.copy.na$workclass)<-c(NA,'Gov', 'Private', 'Self Employed')
levels(census.copy.na$occupation)<-c(NA,"Service" , "Skilled Labor" , "Exec/Specialty", "Labor",  "Sales")
levels(census.copy.na$native.country)<-c(NA,"Asia","Central America", "South America",   "Carabian" , "Europe","North America")
#looking at the chart and graph it seems that if their is no work class their is no occupation. Ei. they go together.
#So my question is when we are trying to impute this data, we should look to build a model that can semi accuratly predict work/occ 
#without the other one as a predictor. Ei: that is some tricky stuff
#also note that it was the most common occupation and worker class are missing toegther. there a few other combos but these are the most prevelent
#EDA

#Age
ggplot(census, aes(y=age))+
  geom_boxplot()

boxplot.stats(census$age)

ggplot(census,aes(x=age,fill=income.class))+
  geom_density(stat = "count")

ggplot(census,aes(x=age, fill=income.class))+
  geom_histogram(position= "fill", bins = 60)

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
scew(census$age)

  #bining age in even width for comparison purposes
  age.labs <- c(paste(seq(0, 80, by = 20), seq(0 + 20 - 1, 100 - 1, by = 20),    #maybe credit site
                    sep = "-"), paste(100, "+", sep = ""))
  census$age.e.bin <- cut(census$age, breaks = c(seq(0, 100, by = 20), Inf), labels = age.labs, right = FALSE)
  
  #Normalize age
  max(census$age) #90
  min(census$age) #17
  census$age.norm<- ((census$age-17)/(90-17))
 

  #Workclass
table(census$workclass)
ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "stack")  #alternate stack and fill to look at count and normalized graphs
ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "fill")
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

  #dummy
gov<-unemployed<-private<-self<- c(rep(0, length(census$age))) #unemployed is the base
  for(i in 1:length(census$age)){
    if(census$workclass[i]== "Gov")gov[i]<-1
    if(census$workclass[i]== "Private")private[i]<-1
    if(census$workclass[i]== "Self emplyed")self[i]<-1
  }
census$workclass.gov<-gov
census$workclass.private<-private
census$workclass.self<-self

#fnlwgt means the # of people this observation represents. Not relevant
ggplot(census, aes(x= fnlwgt, fill= income.class))+
  geom_histogram(position = "fill")

#Education
table(census$education)
census$education<-factor(census$education, levels=c("No HS Degree","HS Grad", "Undergrad", "Graduate", "Prof-school", 'Doctoral'))
ggplot(census, aes(x= education))+
  geom_bar(fill="lightblue")
ggplot(census, aes(x= education, fill=income.class))+
  geom_bar(position="fill")
ggplot(census, aes(x= education, fill= sex))+
  geom_bar(position= "stack")
ggplot(census, aes(x=education, fill= race))+
  geom_bar(position= "fill")

  #Dummy edu
census$education.num<- c(rep(0,length(census$education.num)))   #however, is a doctoral and prof school the same distance as no hs degree and hs degree?
  levels(census$education)
  for(i in 1:length(census$age)){
    if(census$education[i]=="No HS Degree")census$education.num[i]<-1
    if(census$education[i]== "HS Grad")census$education.num[i]<-2
    if(census$education[i]== "Undergrad")census$education.num[i]<-3
    if(census$education[i]== "Doctoral")census$education.num[i]<-6
    if(census$education[i]== "Graduate")census$education.num[i]<-4
    if(census$education[i]== "Prof-school")census$education.num[i]<-5
  }
  nohs<-hsgrad<-undergrad<-graduate<-profschool<-Doc<- c(rep(0,length(census$age)))
  
  for(i in 1:length(census$age)){
    if(census$education.num[i]==2)hsgrad[i]<-1
    if(census$education.num[i]==3)undergrad[i]<-1
    if(census$education.num[i]==4)graduate[i]<-1
    if(census$education.num[i]==5)profschool[i]<-1
    if(census$education.num[i]==6)Doc[i]<-1
  }
   #NO highschool is base
   census$education.hs.grad<-hsgrad    
   census$education.undergrad<-undergrad
   census$education.graduate<-graduate
   census$education.profschool<-profschool
   census$education.Doctoral<-Doc
   

#Marital.status
table(census$marital.status)
ggplot(census, aes(x= marital.status,fill= income.class))+
  geom_bar(position = "fill")
ggplot(census, aes(age.e.bin, fill= marital.status ))+
  geom_bar(position = 'stack')
census%>% filter(age<20)%>%filter(marital.status== "Married")

  #Dummy
levels(census$marital.status)
marital.status.married<-marital.status.divorced<-marital.status.widowed<-c(rep(0,length(census$age)))
  for(i in 1:length(census$age)){
    if(census$marital.status[i]== "Married")marital.status.married[i]<-1
    if(census$marital.status[i]== "Divorced")marital.status.divorced[i]<-1
    if(census$marital.status[i]== 'Widowed')marital.status.widowed[i]<-1
  }
  #not married is base
  census$marital.status.married<-marital.status.married
  census$marital.status.divorced<-marital.status.divorced
  census$marital.status.widowed<-marital.status.widowed
#Relationship status(might be coorelated to marital status)
  ggplot(census, aes(x= relationship, fill= income.class))+   
    geom_bar(position = "stack")

  ggplot(census, aes(x=relationship, fill= marital.status))+
    geom_bar(position= "stack")
  
  ggplot(census, aes(x=relationship, fill= sex))+
    geom_bar(position= "stack")
  
  x<-table(census$marital.status, census$relationship) 
  chisq.test(x) #This might be funky
          #Due to the small p value we can reject the null hypothesis that relationship and marital.status are independent
          #Since the values for those not married and married are captured earlier in the marital.status this caragory will be ommited

#capital income
ggplot(census, aes(capital.gain, fill= income.class))+
  geom_histogram(position= 'stack')
ggplot(census, aes(capital.loss, fill= income.class))+
  geom_histogram(position= 'stack')
boxplot(census$capital.gain)
boxplot(census$capital.loss)
#both these data sets are right skewed, probably not suited for regression.
qqnorm(census$capital.gain, col= "blue") 
qqline(census$capital.gain, col="red")
qqnorm(census$capital.loss, col= "blue") 
qqline(census$capital.loss, col="red") 

scew(census$capital.gain)

#this looks like definitly should not include this in a regression
census%>%filter(capital.gain>50000)%>%filter(income.class== " <=50K.")
census%>%filter(capital.gain>50000)%>%filter(income.class== " >50K.")
#lots of 99999 capital gains. Are they a data miss entry? all have above 50k. Does the system max out at 99999 capital gains
#might be best to just bin or leave alone

summary(census$capital.loss)
#max loss was 3770.0 seems okay. No red flags.

capital.gain.yes<- census$capital.gain>0
prop.table(table(census$income.class,capital.gain.yes),2) #61% of those with>50k  have capital gain
summary(census$income.class)
capital.loss.yes<- census$capital.loss>0
prop.table(table(census$income.class,capital.loss.yes),2) #About a 50-50 split of those with >50k have capital losses, not very predictivr

capital.gain.or.loss <- (census$capital.gain>0|census$capital.loss>0) #looks pretty even when combining capital loss/ gains prob not useful
prop.table(table(census$income.class, capital.gain.or.loss),2)

ggplot(census, aes(x= capital.gain.or.loss, fill= income.class))+
  geom_bar(position= "fill")
cap.gain<-cap.loss<- c(rep(0, length(census$age)))
for(i in 1:length(census$age)){
  if(capital.gain.yes[i]== "TRUE")cap.gain[i]<-1
  if(capital.loss.yes[i]== "TRUE")cap.loss[i]<-1
}
census$capital.gain.yes<- cap.gain   #extremly skewed but might be interesting to see this data used in decision trees.
census$capital.loss.yes<-cap.loss     #We are also loseing the predictive power of capital.gains over 50k making over 50k

ggplot(census, aes(x=sex, fill= capital.gain.yes))+
  geom_bar(position= 'stack')

#occupation
table(census$occupation)
census %>% filter(occupation== " Adm-clerical") %>% filter(income.class== " >50K.")%>% count()
census %>% filter(occupation== " Exec-managerial")%>% filter(income.class== " >50K.")%>%count()
940/2020 #46.5% exec make over 50k
261/1841 #14% of admin assistents make over 14%, do not combine
oc.t<-table(census$occupation, census$income.class)
rowSums(oc.t)
colSums(oc.t)
ggplot(census, aes( x=occupation, fill= income.class))+ 
  geom_bar(position = "dodge")

table(census$income.class, census$occupation)
table(census$occupation)
ggplot(census, aes( x=occupation, fill= income.class))+
  geom_bar(position = "dodge")
ggplot(census, aes( x=occupation, fill= income.class))+
  geom_bar(position = "fill")
  #dummy, 0=?
levels(census$occupation)
occ.service<-occ.skilledlabor<-occ.exec.spec<-occ.labor<-occ.sales<- c(rep(0,length(census$age)))
for(i in 1:length(census$age)){
  if(census$occupation[i]=="Service")occ.service[i]<-1
  if(census$occupation[i]== "Skilled Labor")occ.skilledlabor[i]<-1
  if(census$occupation[i]== "Labor")occ.labor[i]<-1
  if(census$occupation[i]== 'Exec/Specialty')occ.exec.spec[i]<-1
  if(census$occupation[i]== 'Sales')occ.sales[i]<-1
}
census$occupation.service<-occ.service
census$occupation.skilled.labor<-occ.skilledlabor
census$occupation.labor<-occ.labor
census$occupation.Exec.Specialty<-occ.exec.spec
census$occupation.Sales<-occ.sales

#Gender
table(census$income.class, census$sex)
barplot(table(census$income.class,census$sex))
gt1<- table(census$income.class,census$sex)
gt2<- gt1
gt2[1,] <- (gt2[1,]/margin.table(gt1,2))*100
gt2[2,]<- (gt2[2,]/margin.table(gt1,2))*100
barplot(gt2, legend= levels(census$income.class))
gt2

ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="fill")

ggplot(census, aes(race, fill= sex))+
  geom_bar(position="fill")

ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="stack") #much fewer females than males. Possible bias: calculated differently from the standard pouplation 
levels(census$income.class)
census%>% filter(race== "White")%>% filter(sex==" Male")%>% count()
census%>% filter(race== "White")%>% filter(sex==" Male")%>% filter(income.class== " >50K.")%>%count()
2976/9561
#dummy
levels(census$sex)
sex.male<-rep(0,length(census$age))
for(i in 1:length(census$age)){
if(census$sex[i]=="Male")sex.male[i]<-1
}
census$sex.male<-sex.male
# if you are white and male you have a 31% chance of having over 50K a year,
#compared to having a standard of .236 for the total relative 31% increase
#over half are white males? Does this mean a potential bias
#It looks like males have a higher percent earning more than 50K than femals

#Race
  #race data set has a huge speration between white and other races, makes sence as white is the majority in America
race.table<-table(census$income.class, census$race)
margin.table(race.table,2)

rt1<-table(census$income.class, census$race)
rt2<- rt1
rt2[1,] <- (rt2[1,]/margin.table(rt1,2))*100
rt2[2,]<- (rt2[2,]/margin.table(rt1,2))*100
barplot(rt2)
rt2

ggplot(census, aes(race, fill= income.class))+
  geom_bar(position = "stack")
levels(census$race)
race.white<-race.indian<-race.asian<-race.black<- c(rep(0,length(census$age)))
for(i in 1:length(census$age)){
  if(census$race[i]=="White")race.white[i]<-1
  if(census$race[i]=="Black")race.black[i]<-1
  if(census$race[i]=='Asian')race.asian[i]<-1
  if(census$race[i]=='Indian')race.indian[i]<-1
}
census$race.white<-race.white
census$race.black<-race.black
census$race.asian<-race.asian
census$race.indian<-race.indian
#white looks like to be a deciding factor along with asia/ pacific
  #other is the base variable
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


census%>% filter(hours.per.week.bin== "less than 40hrs")%>% filter(sex== " Male")%>% count()
census%>% filter(hours.per.week.bin== "less than 40hrs")%>% filter(sex== " Female")%>% count()
table(census$sex)
2082/5421 #38% of females work less than 40hrs per week
1842/10860 #17% of males work less than 40hrs per week 
ggplot(census, aes(age.e.bin, fill= hours.per.week.bin))+ 
  geom_bar(position = "fill")
census%>% filter(hours.per.week== 0)%>% count() 
#The data does not include people who do not work 0 hours. However, it does include unemployed people, Is it assumed the hours they work are froma  previous job?



#Native Country
table(census$native.country)
ggplot(census, aes(x=native.country))+
  geom_bar()
ggplot(census, aes(x=native.country, fill= income.class))+
  geom_bar(position = "fill")
ggplot(census, aes(x=native.country, fill= sex))+
  geom_bar(position= "fill")
ggplot(census, aes(x=native.country, fill= workclass))+
  geom_bar(position= "fill")

country.asia<-country.CA<- country.SA<-country.Carabian<- 
  country.Eurpoe<-country.north.america<-country.Euro<-c(rep(0, length(census$age)))
for(i in 1:length(census$age)){
  if(census$native.country[i]=="Asia")country.asia[i]<-1
  if(census$native.country[i]=="Central America")country.CA[i]<-1
  if(census$native.country[i]== "South America")country.SA[i]<-1
  if(census$native.country[i]== "Carabian")country.Carabian[i]<-1
  if(census$native.country[i]== "Europe")country.Euro[i]<-1
  if(census$native.country[i]== "North America")country.north.america[i]<-1
}
census$native.country.asia<- country.asia
census$native.country.centeral.america<-country.CA
census$native.country.south.america<-country.SA
census$native.country.Carabian<-country.Carabian
census$native.country.Euro<-country.Euro
census$native.country.north.america<-country.north.america

#income.factor
income.class.over50k<- c(rep(0,length(census$age)))
for(i in 1:length(census$age)){
  if(census$income.class[i]==">50K")income.class.over50k[i]<-1
}
census$income.class.over50k<- income.class.over50k

