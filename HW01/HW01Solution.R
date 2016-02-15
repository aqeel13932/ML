#Remove Everythign 
#First(a)
rm(list=ls())
#read the data file
data <- read.csv('flag.data',header = FALSE)
#Add Columns Names
colnames(data)<- c("name", "landmass", "zone", "area", "population", "language", "religion", 
                   "bars", "stripes", "colours", "red", "green", "blue", "gold", "white", "black",
                   "orange", "mainhue", "circles", "crosses", "saltires", "quarters", "sunstars", 
                   "crescent", "triangle", "icon", "animate", "text", "topleft", "botright")
#Convert Numbers to Labels
#Convert Landmass
data$landmass <- factor(data$landmass,c(1:6),c("N.America", "S.America", "Europe", "Africa", "Asia", "Oceania"))
#Convert zone
data$zone <- factor(data$zone,c(1:4),c('NE','SE','SW','NW'))

#Concert Language
data$language<- factor(data$language,c(1:10),c('English', 'Spanish', 'French','German','Slavic',
                                               'Other Indo-European', 'Chinese', 'Arabic',
                                               'Japanese/Turkish/Finnish/Magyar', 'Others'))
#Convert Religion

data$religion = factor(data$religion,c(0:7),c('Catholic', 'Other Christian','Muslim','Buddhist',
                                              'Hindu','Ethnic','Marxist','Others'))
#religion barplot 
barplot(table(data$religion))
#landmass barplot
barplot(table(data$landmass))
#language barplot
barplot(table(data$language))

#First (B)
#First Isolate the data I want to work on for this question.
importadata = subset (data,landmass=="Europe" | landmass =="Africa")

#drop levels of landmass (landmass other than(Europe , Africa) won't show up in the plot)
importadata$landmass <- droplevels(importadata$landmass)

#plot area vs landmass
boxplot(area ~ landmass,data=importadata)

#plot popluation vs landmass
boxplot(population ~landmass,data=importadata)

#plot area vs religion
boxplot(area ~religion,data,ylim=c(0,3200))

#plot population vs religion
boxplot(population ~religion,data,ylim=c(0,680))

#plot population vs religion (minimze scale)
boxplot(population ~religion,data,ylim=c(0,340))

#Number of Hindu Countries
length(subset(data,religion=='Hindu')$landmass)
#Number of Marxist Countries
length(subset(data,religion=='Marxist')$landmass)
#Number of Muslim Countries
length(subset(data,religion=='Muslim')$landmass)

#Basic Variables
qlvls = c(1:9)/10
dataEurope<-subset(data,landmass=="Europe")
dataAfrica<-subset(data,landmass=="Africa")
#quantile area 
quant.Europe.area<-quantile(dataEurope$area,qlvls)
quant.Africa.area<-quantile(dataAfrica$area,qlvls)

#quantile population
quant.Europe.population<-quantile(dataEurope$population,qlvls)
quant.Africa.population<-quantile(dataAfrica$population,qlvls)

#quantile density
EuropeDensity <- (dataEurope$population*1000000)/(dataEurope$area*1000)
AfricaDensity <- (dataAfrica$population*1000000)/(dataAfrica$area*1000)
#clear Empty values
EuropeDensity<-EuropeDensity[!is.nan(EuropeDensity)]
AfricaDensity<-AfricaDensity[!is.nan(AfricaDensity)]
quant.Europe.density<-quantile(EuropeDensity,qlvls)
quant.Africa.density<-quantile(AfricaDensity,qlvls)

#qqplot 
qqplot(quant.Europe.area,quant.Africa.area)
qqplot(quant.Europe.population,quant.Africa.population)
qqplot(quant.Europe.density,quant.Africa.density)

#Cover
Marxist.Cover = nrow(subset(data,religion=="Marxist"))
Muslims.Cover = nrow(subset(data,religion=="Muslim"))
English.Cover = nrow(subset(data,language=="English"))

#Support
Marxist.Support = nrow(subset(data,religion=="Marxist" & red=="1"))
Muslim.Support = nrow(subset(data,religion=="Muslim" & green=="1"))
English.Support = nrow(subset(data,language=="English"& saltires>0))

#Relative Support
Marxist.RelativeSupport = Marxist.Support/nrow(data)*100
Muslim.RelativeSupport = Muslim.Support/nrow(data)*100
English.RelativeSupport = English.Support/nrow(data)*100

#Confidence 
Marxist.Confidence = Marxist.Support/Marxist.Cover
Muslims.Confidence = Muslim.Support/Muslims.Cover
English.Confidence = English.Support/English.Cover
English.Cover
English.Support
English.RelativeSupport
English.Confidence


#First (e)
#analyzing saltires data
testdata <- subset(data,saltires>0)
#trying rules
saltires.English.Support <- nrow(subset(data,language=="English" &saltires>0))
saltires.English.Confidence <-saltires.English.Support /nrow(subset(data,language=="English" ))
saltires.OtherChristian.Support<-nrow(subset(data,religion =="Other Christian"&saltires>0))
saltires.OtherChristian.Confidence<-saltires.OtherChristian.Support/nrow(subset(data,religion=="Other Christian"))
saltires.Zone.Support <-nrow(subset(data,zone=="NW"& saltires>0))
saltires.Zone.Confidence<-saltires.Zone.Support/ nrow(subset(data,zone=="NW"))
#analyzing crosses
testdata<-subset(data,crosses>0)

#trying rules
crosses.OtherChristian.Support<-nrow(subset(data,religion=="Other Christian"& crosses>0))
crosses.OtherChristian.Confidence<-crosses.OtherChristian.Support/nrow(subset(data,religion=="Other Christian"))

crosses.English.Support<-nrow(subset(data,language=="English"& crosses>0))
crosses.English.Confidence<-crosses.OtherChristian.Support/nrow(subset(data,language=="English"))

crosses.blue.Support<-nrow(subset(data,botright=="blue"&crosses>0))
crosses.blue.Confidence<-crosses.blue.Support/nrow(subset(data,botright=="blue"))

crosses.zone.Support <-nrow(subset(data,zone=="NE"&crosses>0))
crosses.zone.Confidence<-crosses.zone.Support/nrow(subset(data,zone=="NE"))

#analyzing Sunsorstars
testdata<-subset(data,sunstars>0)

#Check By Language
table(testdata$language)
table(testdata$language)/table(data$language)

#Check By landmass
table(testdata$landmass)
table(testdata$landmass)/table(data$landmass)

#Check By religion
table(testdata$religion)
table(testdata$religion)/table(data$religion)

#Check By Zone
table(testdata$zone)
table(testdata$zone)/table(data$zone)

#First (f)
testdata <- subset(data,saltires>0)
saltires.EnglishAndOtherChristian.Support <-nrow(subset(data,language=="English"&religion=="Other Christian" & saltires>0))
saltires.EnglishAndOtherChristian.Confidence <-saltires.EnglishAndOtherChristian.Support /nrow(subset(data,language=="English"&religion=="Other Christian"))

testdata<-subset(data,crosses>0)
crosses.EnglishAndSW.Support<-nrow(subset(data,zone=="SW"&language=="English"&crosses>0))
crosses.EnglishAndSW.Confidence <- crosses.EnglishAndSW.Support/nrow(subset(data,zone=="SW" & language =="English"))
testdata<-subset(data,sunstars>0)
sunsorstars.EnglishAndOceania.Support <-nrow(subset(data,language=="English"&landmass=="Oceania"&sunstars>0))
sunsorstars.EnglishAndOceania.Confidence<-sunsorstars.EnglishAndOceania.Support/nrow(subset(data,language=="English"&landmass=="Oceania"))
#First(G)
#I USED THE CODE FROM PREVIOUS YEAR PRACTICE SESSION JUST CHANGED THE PARAMETERS
# Elements of a confusion matrix
true.positives = nrow(subset(data, green==1 & religion == "Muslim"))
true.negatives = nrow(subset(data, green ==0 & religion != "Muslim"))
false.positives = nrow(subset(data, green ==0 &  religion == "Muslim"))
false.negatives = nrow(subset(data,  green==1& religion != "Muslim"))

confusion.matrix <- rbind(c(true.positives, false.negatives), c(false.positives, true.negatives))
colnames(confusion.matrix) <- c("Predicted positives", "Predicted negatives")
rownames(confusion.matrix) <- c("Labelled positives", "Labelled negatives")
confusion.matrix
#Opposite
true.positives = nrow(subset(data, green==0 & religion != "Muslim"))
true.negatives = nrow(subset(data, green ==1 & religion == "Muslim"))
false.positives = nrow(subset(data, green ==1 &  religion != "Muslim"))
false.negatives = nrow(subset(data,  green==0& religion == "Muslim"))

confusion.matrix <- rbind(c(true.positives, false.negatives), c(false.positives, true.negatives))
colnames(confusion.matrix) <- c("Predicted positives", "Predicted negatives")
rownames(confusion.matrix) <- c("Labelled positives", "Labelled negatives")
confusion.matrix

#Second (a)
GetSamples<-function(X,n){
  S <- sample(x =  X,size =  n,replace = TRUE)
  print(min(S))
  print (mean(S))
  return (S)
}
values<-c(1:10000)
GetSamples(values,10)

#Second(b)
values<-c(1:10000)
#write the function without printig not to spam
GetSamples2<-function(X,n){
  S <- sample(x =  X,size =  n,replace = TRUE)
  return (S)
}
#define value containers 
meanVector<-c(1:1000)
minVector<-c(1:1000)
meav <- vector(mode="numeric", length=0)
miav <- vector(mode="numeric", length=0)
mesd <- vector(mode="numeric", length=0)
misd <- vector(mode="numeric", length=0)
for (n in c(1,10,100,1000,10000))
{

for (i in 1:1000)
{
  #Get sample 
  #n = 1,10,100,1000,10000 
  currentSample = GetSamples2(values,n)
  #calculate current min,mean
  meanVector[i] = mean(currentSample)
  minVector[i]=min(currentSample)
}
  meav <- c(meav,mean(meanVector))
  miav <- c(miav,mean(minVector))
  mesd <- c(mesd,sd(meanVector))
  misd <- c(misd,sd(minVector))

}
#THIS CODE WHERE COPIED FROM THIS SOURCE :http://stackoverflow.com/questions/15063287/add-error-bars-to-show-standard-deviation-on-a-plot-in-r
#I CHANGED THE VALUES TO FIT WHATS REQUESTED IN THE QUESTION
#prepare data for (mean , min one of them each time )
d = data.frame(
  x  = c(1,10,100,1000,10000)
  , y  = miav
  , sd = misd
)

##install.packages("Hmisc", dependencies=T)
library("Hmisc")

# add error bars (without adjusting yrange)
plot(d$x, d$y, type="n",ylim=c(0,8000),xlim = c(0,1000),xlab = "Size",ylab = "Average")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=T, pch=1, cap=.1)
)

#second (c)
values<-c(1:10000)
#write the function without printig not to spam
GetSamples2<-function(X,n){
  S <- sample(x =  X,size =  n,replace = FALSE)
  return (S)
}
#define value containers 
meanVector<-c(1:1000)
minVector<-c(1:1000)
meav <- vector(mode="numeric", length=0)
miav <- vector(mode="numeric", length=0)
mesd <- vector(mode="numeric", length=0)
misd <- vector(mode="numeric", length=0)
for (n in c(1,10,100,1000,10000))
{
  
  for (i in 1:1000)
  {
    #Get sample 
    #n = 1,10,100,1000,10000 
    currentSample = GetSamples2(values,n)
    #calculate current min,mean
    meanVector[i] = mean(currentSample)
    minVector[i]=min(currentSample)
  }
  meav <- c(meav,mean(meanVector))
  miav <- c(miav,mean(minVector))
  mesd <- c(mesd,sd(meanVector))
  misd <- c(misd,sd(minVector))
  
}
#THIS CODE WHERE COPIED FROM THIS SOURCE :http://stackoverflow.com/questions/15063287/add-error-bars-to-show-standard-deviation-on-a-plot-in-r
#I CHANGED THE VALUES TO FIT WHATS REQUESTED IN THE QUESTION
#prepare data for (mean , min one of them each time )
d = data.frame(
  x  = c(1,10,100,1000,10000)
  , y  = miav
  , sd = misd
)

##install.packages("Hmisc", dependencies=T)
library("Hmisc")

# add error bars (without adjusting yrange)
plot(d$x, d$y, type="n",ylim=c(0,8000),xlim = c(0,1000),xlab = "Size",ylab = "Average")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=T, pch=1, cap=.1)
)
meav
mesd
miav
misd
