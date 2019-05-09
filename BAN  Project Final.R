#------------------------------------------------------------------------------------#
#---------------------BAN-340---Final---Project--------------------------------------#
#------------------------------------------------------------------------------------#
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
library('ROCR')
library(gridExtra)
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
table(missing.values.complete$education)
table(missing.values.complete$sex)
table(missing.values.complete$relationship)
table(missing.values.complete$income.class)
median(missing.values.complete$age)
median(census$age)
(333/2065) #16% of the missing values make over 50k, compared to the 25% overal in the dataset. This could be due to the lower class
#earners being less willing to share their occupation and workclass.
rm(missing.values.native,missing.values.occupation, missing.values.workclass)
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

age.den<-ggplot(census,aes(x=age,fill=income.class))+
  geom_density(stat = "count", colour='black')+
  theme_minimal()+
  ggtitle( 'Age frequency compared to income'
  )
age.den  

age.norm<-ggplot(census,aes(x=age, fill=income.class))+
  geom_histogram(position= "fill", bins =36 , show.legend = FALSE)+
  theme_minimal()+
  scale_x_continuous(breaks=c(20,25,30,35,40,45,50,55,60,65,70,75,80,85,90))+
labs(title='Ratio of Age to Income Class',
     y= 'Ratio')

grid.arrange(age.den,age.norm,nrow= 2)

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
wrk.count<- ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "stack", colour= 'black')+
  scale_y_continuous(breaks = c(5000,22000))+
  theme_minimal()+
  ggtitle('Frequencies of Workclass')+
  coord_flip()#alternate stack and fill to look at count and normalized graphs
wrk.norm<- ggplot(census, aes(x= workclass, fill= income.class))+
  geom_bar(position= "fill",colour= 'black', show.legend = FALSE)+
  theme_minimal()+
  scale_y_continuous(breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs( y= 'ratio',
        title= 'Normalized Workclass'
  )+ coord_flip()
grid.arrange(wrk.count, wrk.norm, nrow= 1)
#These two graphs give us an idea of how the workclass of a person might effect his/hers earning prospects
#It seems that the majority of the data fall in the Private sector but those who work in government and self have better 
#have a better percentage of earning over 50k. However, I doubt this has predictive capabilities as the private class
#is so large that it probably includes many lowerpaying jobs as well as more higher paying jobs. I do 
workclass.table<- table(census$workclass,census$income.class)
work.inc.margin<- round(prop.table(workclass.table, margin = 1),2)*100
workclass.table
work.inc.margin
#This table shows the actual percentages of each specific workclass's  
ggplot(census, aes(x=age.e.bin, fill=workclass))+
  geom_bar(position = 'dodge', colour='black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,250,1000,2000,3000,7000,12500))+
  labs(y= 'ratio',
       x= 'age bin',
       title= 'workclass by age')
ggplot(census, aes(x=age.e.bin, fill=workclass))+
  geom_bar(position = 'fill', colour='black')+
  scale_y_continuous(breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  theme_minimal()+
  labs(y= 'ratio',
       x= 'age bin',
       title= 'workclass by age')
#This graph does not show us anything to do with income class but it is interesting to the increase of percentage of self employed people.

ggplot(census, aes(x=workclass, fill=sex))+
  geom_bar(position = 'fill', colour= 'black')+
scale_y_continuous(breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  theme_minimal()+
  labs( y= 'ratio',
        title= 'ratio of gender in workclass')
#here we can see the ratio of males and females in each respective work class. Nothing seems out of the ordinary
#and looks like the ratios are about what you would expect for each workclass based on the proportion of females in
#the entire dataset. 

#fnlwgt means the # of people this observation represents.
fwl.hist<-ggplot(census, aes(x= fnlwgt, fill= income.class))+
  geom_histogram(position = "stack", colour= 'black')+
  theme_minimal()+
  labs(title= 'Count of FNLWGT')

fwl.norm<-ggplot(census, aes(x= fnlwgt, fill= income.class))+
  geom_histogram(position = "fill", colour= 'black', show.legend = FALSE)+
  theme_minimal()+
  labs(title= 'Ratio of income calss to FNLWGT')
  
  grid.arrange(fwl.hist, fwl.norm, nrow= 1)
#The business objective stated at the start of this model is to predict wether a person makes over/under 50K. Thought this variable
# might have some predictive power it is unlikly that new data would come with this number thus reducing its use. For this reason 
# it will be left out of the modeling section. 

#Education
table(census$education)
edu.freq<-ggplot(census, aes(x= education, fill= income.class))+
  geom_bar(position = 'stack', colour= 'black')+
  theme_minimal()+
  labs(title= 'Education Frequency')+
  scale_y_continuous(breaks = c(0,500,1618,3731,1618,5019,18762))
  
edu.norm<-ggplot(census, aes(x= education, fill=income.class))+
  geom_bar(position="fill", colour= 'black', show.legend = FALSE)+
  theme_minimal()+
  scale_y_continuous(breaks = c(.068,.19,.422,.566,.75))+
  labs(title= 'Ratio of Education to Income Class',
       y= 'ratio')

grid.arrange(edu.freq,edu.norm, nrow= 1)
#about what you would expect. Those with higher education levels tend to make over 50k more often.

ggplot(census, aes(x= education, fill= sex))+
  geom_bar(position= "stack")
ggplot(census, aes(x=education, fill= workclass))+
  geom_bar(position= "fill")
                      #note: due to the re-leveling edu.num is none-representative of level.
                            #furthermore it is best to level indicators at 0-1 as the difference of a No-Hs and HS and undergrad-grad is subjective
#Marital.status
table(census$marital.status)

mar.stack<-ggplot(census, aes(x= marital.status,fill= income.class))+
  geom_bar(position = "stack", colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks= c(822,4206,10617,14400))+
  labs(x= 'marital status',
       title= 'marital status frequency')+ 
  coord_flip()

mar.norm<- ggplot(census, aes(x= marital.status,fill= income.class))+
  geom_bar(position = "fill", colour= 'black', show.legend = FALSE)+
  theme_minimal()+
  scale_y_continuous(breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title= 'ratio of marital status to income',
       y= 'ratio')+
  coord_flip()

grid.arrange(mar.stack,mar.norm, nrow= 1)
#Relationship status
rela.feq<-ggplot(census, aes(x= relationship, fill= income.class))+   
  geom_bar(position = "stack", colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,880,3198,4446,7703,13818))+
  labs(title= 'Relationship Frequency')

rela.norm<-ggplot(census, aes(x= relationship, fill= income.class))+   
  geom_bar(position = "fill", colour= 'black',show.legend = FALSE)+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1,.45,.11,.05))+
  labs(title= 'Ratio of Relationship to Income Class')
grid.arrange(rela.feq,rela.norm, nrow=1)

ggplot(census, aes(x=relationship, fill= marital.status))+
  geom_bar(position= "fill")
ggplot(census, aes(x=relationship, fill= sex))+
  geom_bar(position= "stack")
#It will probably best to leave out one of relationship or marital status due to the similarities. Also note
#how the married amount between relationship and marital status is differnt 14400 in marital vs 17016
#I do not really see why this is though it probably should be investigated further. ALso this both of these variables will
#most likly have strong predictive power.

#--------capital income-----------------#
gain.hist<-ggplot(census, aes(capital.gain, fill= income.class))+
  geom_histogram(position= 'stack', colour= 'black')+
  scale_y_continuous(breaks = c(0,27513,500,1000,5750))+
  theme_minimal()+
  labs(title= 'Capital Gains Frequency',
       x= 'Capital Gains')

loss.hist<-ggplot(census, aes(capital.loss, fill= income.class))+
  geom_histogram(position= 'stack', colour= 'black', show.legend = FALSE)+
  scale_y_continuous(breaks = c(28624,1000,500,0,6750))+
  theme_minimal()+
  labs(
    x= "Captial Loss",
    title='Capital Loss Frequency'
  )
grid.arrange(gain.hist,loss.hist, nrow=1)
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
boxplot(census$capital.gain)
boxplot(census$capital.loss)
qqnorm(census$capital.gain, col= "blue") 
qqline(census$capital.gain, col="red")
qqnorm(census$capital.loss, col= "blue") 
qqline(census$capital.loss, col="red") 
#lots of 99999 capital gains. Are they a data miss entry? all have above 50k. Does the system max out at 99999 capital gains?
summary(census$capital.loss)
summary(census$capital.gain)
#max loss was 3770.0 seems okay. No red flags.
prop.table(table(census$income.class,census$capital.gain.yes),2) #62% of those with>50k  have capital gain
summary(census$income.class)
prop.table(table(census$income.class,census$capital.loss.yes),2) #About a 50-50 split of those with >50k have capital losses, not very predictivr

capital.gain.or.loss <- (census$capital.gain>0|census$capital.loss>0) #seem more likly that a person who makes over 50k has money in the market.
prop.table(table(census$income.class, capital.gain.or.loss),2)

ggplot(census, aes(x= capital.gain.or.loss, fill= income.class))+
  geom_bar(position= "fill", colour= 'black')+
  scale_y_continuous(breaks = c(0,.196,.586,1))+
  theme_minimal()+
  labs(x= 'Capital Gain or Loss',
       y= 'Ratio',
       title= 'Ratio of Captial Gain/loss to income')+
  coord_flip()

table(capital.gain.or.loss)
#This looks to have some prediction power but this only applies for around 4000 people out of 26092
#occupation
occ.norm<-ggplot(census, aes( x=occupation, fill= income.class))+ 
  geom_bar(position = "fill", colour= 'black')+
  scale_y_continuous(breaks = c(.13,.22,.47), sec.axis = sec_axis(~.,breaks = c(.06,.17,.27,.3)))+
  theme_minimal()+
  labs(y= 'ratio',
       title= 'Ratio of occupation to income class')

occ.freq<-ggplot(census, aes( x=occupation, fill= income.class))+ 
  geom_bar(position = "stack", colour= 'black', show.legend = FALSE)+
  scale_y_continuous(breaks = c(0,3712,5586,8001), sec.axis = sec_axis(~.,breaks = c(5665,2605,3565,911)))+
  theme_minimal()+
  labs(y= 'Count',
       title= 'Frequency of Occupation')

grid.arrange(occ.freq, occ.norm, nrow=1 )

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
gt2 #percentages

gend.norm<-ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="fill", colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(.1138,0,1), sec.axis = sec_axis(~.,breaks = c(.3144,0,1)))+
  labs(y= 'ratio',
       x= 'Gender',
       title= 'Ratio of Gender to Income class')+
  coord_flip()

gen.freq<-ggplot(census, aes(sex, fill= income.class))+
  geom_bar(position="stack", colour= 'black',show.legend = FALSE)+
  scale_y_continuous(breaks = c(1109,9743), sec.axis = sec_axis(~.,breaks = c(6382,20302)))+
  theme_minimal()+
  labs(
    x= 'Gender',
    y= 'Count',
    title= 'Frequency of Gender'
  )+ coord_flip()

grid.arrange(gend.norm, gen.freq, nrow=2)

#much fewer females than males. Could also be due to more females falling into the sames groups. Using the fnlwgt and comparing those numbers to males might be more equal
 f<-census%>%filter(sex== 'Female')
 m<- census%>% filter(sex== 'Male')
sum(f$fnlwgt)
sum(m$fnlwgt)
sum(f$fnlwgt)/(sum(f$fnlwgt,m$fnlwgt)) #almost the exact percentage of 31% female and 69% males so this is not the case.
 
ggplot(census, aes(race, fill= sex))+
  geom_bar(position="fill")

#Race
race.table<-table(census$income.class, census$race)
race.table #majority are white
margin.table(race.table,2)
rt1<-table(census$income.class, census$race)
rt2<- rt1
rt2[1,] <- (rt2[1,]/margin.table(rt1,2))*100
rt2[2,]<- (rt2[2,]/margin.table(rt1,2))*100
rt2 #race percentages, asians and whites are more likly to make over 50k

race.freq<-ggplot(census, aes(race, fill= income.class))+
  geom_bar(position = "stack", colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(1000,3000), sec.axis = sec_axis(~.,breaks = c(6836,25892)))+
  labs(title= 'Frequency of Race')+ 
  coord_flip()
  

race.norm<-ggplot(census, aes(race, fill= income.class))+
  geom_bar(position = "fill", colour= 'black', show.legend = FALSE)+
  scale_y_continuous(breaks = c(0,.117,.285,1), sec.axis = sec_axis(~.,breaks = c(.091,.264,.13,1)))+
  theme_minimal()+
  labs(
    y= 'ratio',
    title= 'Race to income class ratio'
  )+
  coord_flip()
grid.arrange(race.freq,race.norm)

levels(census$race)
census%>% filter(race== "White")%>% filter(sex=="Male")%>% count()
census%>% filter(race== "White")%>% filter(sex=="Male")%>% filter(income.class== ">50K")%>%count()
5865/18011 #Does not seem that race will be a huge predictor of income

# Also heavily scewed toward white which is expected, also white is pretty high

#hours per week 
ggplot(data= census, aes(y= hours.per.week))+
  geom_boxplot()+
  theme_minimal()+
theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
      plot.title = element_text(hjust = 0.5))+
  labs(y='',
       title= 'Hours per Week')

boxplot.stats(census$hours.per.week)
max(census$hours.per.week)
hrsperweek.out<-census[which(census$hours.per.week>52|census$hours.per.week<33),]
summary(hrsperweek.out)
census[which(census$hours.per.week== 99),]
census%>%filter(income.class==">50K")%>%filter(hours.per.week== 99)
census%>%filter(hours.per.week== 99)%>%count()
census%>%filter(income.class==">50K")%>%filter(hours.per.week== 99)%>%count()
(25/77) #percentage similar to the regular population......
#These values seem interesting with a mix of above and below incomeclass. I wounder if these
#results should be included or removed in the model. I know its possible and people work these hours but
#are these results actually indicutive or was the 99hrs an error in data entry?

qqnorm(census$hours.per.week, col= "blue") 
qqline(census$hours.per.week, col="red") #closser to normality than capital.gains/loss

ggplot(census, aes(x= hours.per.week, fill= income.class))+
  geom_histogram(position = "stack", colour= 'black', bins = 10, binwidth = 10)+
  theme_minimal()+
  scale_y_continuous(breaks = c(17350,0,500,1000,1500,2000))+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,99))+
  labs(y= 'Hours per Week',
       title= 'Hours per Week Frequency')
  

#almost half work 40hrs a week
census$hours.per.week<- as.numeric(census$hours.per.week)
hours.per.week.bin<- cut(census$hours.per.week, breaks = c(0,39, Inf), labels = c("<40hrs", ">=40hrs"))
over.40hrs<- c(rep(0,length(census$age)))
for(i in 1:length(1:length(census$age))){
  if(hours.per.week.bin[i]== ">=40hrs")over.40hrs[i]<-1
}
census$hours.per.week.over40<-over.40hrs

hrs.bin.freq<-ggplot(census, aes(x= hours.per.week.bin, fill= income.class))+
  geom_bar(position = "stack", colour= 'black')+
  scale_y_continuous(breaks = c(6687, 656), sec.axis = sec_axis(~.,breaks = c(23358,6835)))+
  theme_minimal()+
  labs(
    x= 'Hours Per Week',
    title= 'Frequency of over/under hours worked'
  )+coord_flip()

hrs.bin.norm<-ggplot(census, aes(x= hours.per.week.bin, fill= income.class))+
  geom_bar(position = "fill", colour= 'black', show.legend = FALSE)+
  scale_y_continuous(breaks = c(.0981,0,1), sec.axis = sec_axis(~.,breaks = c(.2926,0,1)))+
  theme_minimal()+
  labs(
    x= 'Hours Per Week',
    y= 'Ratio',
    title= 'Ratio of over income.class and over/under 40hrs work weeks ')+ 
  coord_flip()

grid.arrange(hrs.bin.freq,hrs.bin.norm,nrow=2)

#This predictor could help as those who work less than 40hrs do not make over 50k often

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
nat.feq<-ggplot(census, aes(x=native.country))+
  geom_bar(colour= 'black', fill= 'lightgreen')+
  theme_minimal()+
  labs(title= "Location",
       y= 'Frequecy of Location')+
  scale_y_continuous(breaks = c(1500,27572))+
  coord_flip()

nat.norm<-ggplot(census, aes(x=native.country, fill= income.class))+
  geom_bar(position = "fill", colour= 'black')+
  theme_minimal()+
  scale_y_continuous(breaks = c(.321,.255,.076,0,1), sec.axis = sec_axis(~.,breaks = c(.083,.302,0,1)))+
  labs(
    x= 'location',
    y='ratio',
    title= 'Ratio of Native Country to Income' )+
  coord_flip()
grid.arrange(nat.feq, nat.norm)  
  

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
census.full<-census #copy for safe keeping
rm(hrsperweek.out,age.out)
census$age.e.bin<-NULL
census$education.num<-NULL
census$fnlwgt<-NULL
census$marital.status #due to the simularities that relationship captures
census$capital.gain<-NULL
census$capital.loss<-NULL
census$hours.per.week<-NULL
census$native.country
census$workclass<-factor(census$workclass) #dropping '?'
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

#normalize
census.train$age<- minmax.nrom(census.train$age)
census.test$age<- minmax.nrom(census.test$age)

#------------Missing data imputation--------------#
#Here we will look at potentially filling in the 2398 observations missing data points. lets look at the dispersion again
md.pattern(census.copy.na[,c(2,7,14)])
#In most observations if workclass is missing then occupation is missing as well So workclass should not be used as a
#predictor for occupation as well. Though we can use multiple imputation models we will just look at decision trees for imputation

#----C50-----#
x<-census.train[,-c(2,14,5)]
y<- census.train[,2]
work.c50<- C5.0(x,y)
summary(work.c50) #25 percent error We might as well pick private for all missing and achive 25% error
work.c50.predict<- predict.C5.0(work.c50, newdata = census.test[,-c(2,14,5)], type= 'class')
table(census.test$workclass, work.c50.predict)
(40+4336+31)/6000 #abour 27% accuracy for work class

x<-census.train[,-c(2,14,5)]
y<-census.train[,5]
occupation.c50<- C5.0(x,y)
summary(occupation.c50) #51% error, not worth predicting the test set.

x<-census.train[,-c(14,9)]
y<-census.train[,9]
native.c50<- C5.0(x,y)
summary(native.c50) #overly selecting America in selection. Makes sense due to the frequency
native.c50.predict<- predict.C5.0(native.c50, census.test[,-c(14,9)])
table(census.test$native.country,native.c50.predict)
(98+5451+20)/6000 #about 92% error, 
(5505/6000) #91.75% error, means our model did 1% better than just picking North America
#---CART---#
x<-census.train[,-c(5,14)]
work.rpart<- rpart(workclass~.,data = x, method='class')
summary(work.rpart) #Rpart finds that the best way to determin the work class is just by selecting all as workclass

x<-census.train[,-c(2,14)]
occ.rpart<- rpart(occupation~.,data = x, method='class')
summary(occ.rpart)
rpart.plot(occ.rpart)
occ.rpart.predict<- predict(occ.rpart, newdata = census.test[,-c(2,5,14)],type = 'class')
table(occ.rpart.predict, census.test$occupation)
(414+982+1006+215)/6000 #again poor prediction

x<-census.train[,-14]
nativ.rpart<- rpart(native.country~., data = x, method= 'class')
summary(nativ.rpart)
rpart.plot(nativ.rpart)#predicts country based on race
native.rpart.predict<- predict(nativ.rpart, newdata = census.test[,-14], type= 'class')
table(native.c50.predict,census.test$native.country)
(98+5451+20)/6000 # again barly anygain over regular imputation

#Based on these decision trees it is pretty well established that the best way to imput the missing data is by
#selecting the Private for all missing workclass, omiting occupation as their is not good way to predict this with 
#acceptable accuracy. Due to workclass and occupation being often missing in pairs it is probably best to omit these
#results for accuacy and the fact that the sample size is already large enough. For Native country the best way
#to deal with its missing data is by imputing North America for the missing data points and achive 90% accuracy, however
#this will not be done as it will soon be proved that the native.country is not a very meaningfull predictor. 

#----------------------------------#
#-----Modeling Phase---------------#
#----------------------------------#
inc.glm<- glm(income.class.ind~.,data = census.train.dummy, family = 'binomial')
summary(inc.glm)
inc.glm.predict<- predict(inc.glm,newdata = census.test.dummy, type = 'response' )
inc.glm.predict <- ifelse(inc.glm.predict> 0.5,1,0)
table(census.test$income.class.ind,inc.glm.predict)
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

#-- glm model including workclass, race, and native country-----#
inc.glm<- glm(income.class.ind~.,data = census.train.dummy, family = 'binomial')
summary(inc.glm)
anova(inc.glm, test = 'Chisq')
inc.glm.predict<- predict(inc.glm,newdata = census.test.dummy, type = 'response' )
inc.glm.predict <- ifelse(inc.glm.predict> 0.5,1,0)
table(census.test$income.class.ind,inc.glm.predict)
(333+636)/6000 # error rate 16.15%
(333*2)+663 #1329 total error cost
(872)/(872+636) # 57.82% sensitivity
(4159)/(4159+333) # 92.59% specifictivity

#Removing Race and Native Country due to non-significance
census.train.1<- census.train
census.test.1<- census.test
census.train.dummy.1<- census.train.dummy
census.test.dummy.1<- census.test.dummy

census.train.1$native.country<-NULL
census.test.1$native.country<-NULL
census.train.1$race<-NULL
census.test.1$race<-NULL

census.train.dummy.1[, c(19:22,24:27)]<-NULL
census.test.dummy.1[,c(19:22,24:27)]<-NULL

inc.1.glm<- glm(income.class.ind~.,data = census.train.dummy.1, family = 'binomial')
summary(inc.1.glm)
anova(inc.1.glm, test = 'Chisq')
inc.glm.predict.1<- predict(inc.1.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.1 <- ifelse(inc.glm.predict.1> 0.5,1,0)
table(census.test$income.class.ind,inc.glm.predict.1)
(332+645)/6000 #error rate of 16.28%

#We can see that these variables had very little effect in the predictability
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
table(census.test$income.class.ind,inc.glm.predict.2)
(331+643)/6000 #16.23% missclass rate with .5 cut off value
((331*2)+643) #total cost of error 1305
(865/(643+865)) #57.46% sensetivity
(4161)/(331+4161) #92% specificty
inc.glm.predict.2<- predict(inc.2.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.2 <- ifelse(inc.glm.predict.2> 0.55,1,0)
table(census.test$income.class.ind,inc.glm.predict.2)
(265+716)/6000 #16.35% error rate
(265*2)+716 #total error cost of 1246
(792/(792+716)) # sensativity of 52.51%
(4227/(265+4227)) #specifity of 94.1%
inc.glm.predict.2<- predict(inc.2.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.2 <- ifelse(inc.glm.predict.2> 0.6,1,0)
table(census.test$income.class.ind,inc.glm.predict.2)
(217+791)/6000 #overall error
((217*2)+791) #total cost of error 1225
(717/(791+717)) # 47.54% sensitivity
(4275/(4255+217)) #95.59% specificty
inc.glm.predict.2<- predict(inc.2.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.2 <- ifelse(inc.glm.predict.2> 0.65,1,0)
table(census.test$income.class.ind,inc.glm.predict.2)
(178+875)/6000 # 17% error rate
(178*2)+875 #total cost of error 1231 
(663/(663+875)) #43.11% sensitivity
(4314/(178+4314)) #96% specificty
inc.glm.predict.2<- predict(inc.2.glm,newdata = census.test.dummy.1, type = 'response' )
inc.glm.predict.2 <- ifelse(inc.glm.predict.2> 0.7,1,0)
table(census.test$income.class.ind,inc.glm.predict.2)
(139+961)/6000 #overall error rate of 12.39%
((139*2+961)) #total cost of error 1239
(547/(961+547)) #36.27% sensitivity
(4353/(4353+139)) #97% specificty

#Though all these are pretty close but the best model is the one with race,native.country, and workclass removed
#looking back at the EDA the graphs seem to back up the models assumptions. Also note that it does look like some of
#these countries/ races do seem to have a higher percentage in the lower income but due to the balance these variables 
#do not seem to affect prediction. However we will put them back into some of the modeles and compare

#----decision trees-------#

  #----C50---With work class, native race, and race--#
x<- census.train[,-c(13,9)]
y<-census.train[,9]
income.c50<- C5.0(x,y)
summary(income.c50) #error of (15.1%)
income.c50.predict<- predict.C5.0(income.c50, census.test[,-c(13,9)])
table(census.test$income.class,income.c50.predict)
(360+626)/6000   #error of  (16.4%) maybe a little overfitting going on but pretty even
(360*2)+626   #total cost of error: 1346 
882/((882+626)) # of 58% sensitivity
4132/(4132+360) # 92% specifictivity

#---C50--Without native,race, and workclass---#
x<- census.train.1[,-c(6,10)]
y<-census.train.1[,6]
income.c50.1<- C5.0(x,y)
summary(income.c50.1) #error of (15.7%)
income.c50.predict.1<- predict.C5.0(income.c50.1, census.test.1[,-c(6,10)])
table(census.test$income.class,income.c50.predict.1)
(388+600)/6000 #error of 16.4% 
(388*2)+600 #1376 is the total cost of errors 
(908)/(908+600) #sensativity of 60%
(4104/(388+4104)) #91% specifictivity
#As the results show the three variables removed had little to no effect on the efficieny of our model and 
#actually improved the accuracy by .01% also it is important to strive for simplicity when possible

#----C50-- with assigned error values----#
costm<- matrix(c(1,2,1,1),nrow = 2)
x<- census.train.1[,-c(6,10)]
y<-census.train.1[,6]
income.c50.2<- C5.0(x,y, costs = costm)
summary(income.c50.2)
income.c50.predict.2<- predict.C5.0(income.c50.2, census.test.1[,-c(6,10)])
table(census.test$income.class, income.c50.predict.2)
(130+957)/6000 #18.12 error rate
(130*2)+957 #total cost of  error:1217
(551)/(957+551) #sensativity of 36.54%
(4362)/(4362+130) #specificty of 97.11%
  #----CART----#
x<-census.train[,-13]
income.rpart<- rpart(income.class~., data= x)
summary(income.rpart)
rpart.plot(income.rpart)
income.rpart.predict<- predict(income.rpart, census.test, type= 'class')
table(census.test$income.class, income.rpart.predict)
(913+226)/6000 #18.98% error
(216*2)+913 #1345 total cost of error
(595/(595+913)) #39.46% sensitivity
(4266)/(4266+226) #specicfity of 94.97%
  #note how CART does only uses relationship+ education thus no point in re-running without workclass, ect...
 
#----KNN--with workclass, native country, and race---#
x<-census.train.dummy[,-31]
y<-census.test.dummy[,-31]
income.knn.optimal<-c(rep(0,30))
for(i in 1:30){
  income.knn.model<-knn(x,y, cl=census.train$income.class,k=i)
  income.knn.optimal[i]<-100*sum(census.test$income.class == income.knn.model)/6000
}
income.knn.optimal #about 82.5% accuracy at k=15 range, however what about ties are random

x<-census.train.dummy[,-31]
y<-census.test.dummy[,-31]
knn.15<- knn(x,y, cl=census.train$income.class,k=15)
table(knn.15,census.test$income.class)
(452+638)/6000 #error rate of 18.1%
(638*2+452) #total cost of errors: 1728
870/(452+870) #sensitivity 65.81%
4040/(4040+638)#specifictivity of 86.36%

#-----KNN-without work, race, native------#
x<-census.train.dummy.1[,-21]
y<-census.test.dummy.1[,-21]
income.knn.optimal.1<-c(rep(0,30))
for(i in 1:30){
  income.knn.model.1<-knn(x,y, cl=census.train$income.class,k=i)
  income.knn.optimal.1[i]<-100*sum(census.test$income.class == income.knn.model.1)/6000
}
income.knn.optimal.1 #a small positive change
x<-census.train.dummy.1[,-21]
y<-census.test.dummy.1[,-21]
knn.15.1<- knn(x,y, cl=census.train$income.class,k=15)
table(knn.15.1,census.test$income.class)

#---Neural Networks--with race, workclass, and native country--#
set.seed(241) 
x<-census.train.dummy[,-31]
x$income.class<- census.train$income.class
income.nnet<- nnet(income.class~., data = x, size= 10, maxit= 1000)
income.nn.predict<- predict(income.nnet, census.test.dummy, type= 'class')
table(census.test$income.class,income.nn.predict)
(373+601)/6000 #error rate
373*2+601 #1347 unit cost
907/(907+601) # 60% sensitvity
4084/(602+4084) # 87.15% specifictivity

#---Neural Networks--without work, race, and native-------#
set.seed(231)
x<-census.train.dummy.1[,-21]
x$income.class<- census.train$income.class
income.nnet.1<- nnet(income.class~., data = x, size= 10, maxit= 1000)
income.nn.predict.1<- predict(income.nnet.1, census.test.dummy.1, type= 'class')
table(census.test$income.class,income.nn.predict.1)
(357+619)/6000 #results 16.26% accuracy
357*2+619 #1333 unit cost
889/(619+889)# 58.95% sensitivity
4135/(4135+357) # 92.05 specifictivity
