library(dplyr)
library(tidyverse)
library(woeBinning)
library(corrplot)
flight <-  read.csv(file="flight.csv", header=T)
#flight <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
head(flight)

#backup for comparison 
flight_bkup <- flight

#what does the data look like
summary(flight)


#Data cleaning
#Let us remove the NAs / check for duplicates and deal with high levearge poings if any

#Remove NAs - as the number of observations with NAs are very less (5)
#compared to the total observations we have decided to remove them instead of fixing 
which(is.na(flight))
which(! complete.cases(flight_bkup))
flight <- flight[complete.cases(flight), ] 


#remove duplicates - 13 duplicate observations removed
which(duplicated(flight))
flight <- distinct(flight)




# check for high leverage points

all_columns <- colnames(flight)
all_formula_strings <- paste0(all_columns," ~ Canceled")
#par(mfrow = c(1, 4))
for (i in c(2,3,5:8)) {
  boxplot(as.formula(all_formula_strings[i]), data = flight,
          col = rgb(1, 0, 0, alpha = 0.4), 
          xlab = "Canceled", 
          ylab = all_columns[i], 
          main = paste0("Box plot for \n", all_columns[i]),
          border = "black", 
          outpch = 25,      
          outbg = "green",  
          whiskcol = "blue",
          whisklty = 2,    
          lty = 1)
}
#par(mfrow = c(1, 1))

##data observations
#distance which has negative and 0  looks like high leverage point
#schld elapse time of 0 and negative looks like high leverage points
#arr delay and depdeplay are 0 for cancelled flights - this will be not considered in analysis


#remove high leverage points
flight[flight$Distance <= 0,]
flight <- flight[!flight$Distance <= 0,]
flight[flight$Distance >10000,]
flight <- flight[!flight$Distance >10000,]
flight[flight$SchedElapsedTime <=0 ,]
flight <- flight[!flight$SchedElapsedTime <=0 ,]


#################################
#Exploratory Data Analysis
#################################

#how many months data are presented
# we find that observations span across all the 12 months
unique(flight$Month)


#Analyzing the cancellation ratio
table(flight$Canceled)
barplot(prop.table(table(flight$Canceled)) * 100, main = "Cancellation",xlab = "cancelled",  
        ylab = "# flights")

#~80 of flights are not cancelled. We see that the data is highly imbalaced.

# Analyzing correlation matrix we can see that there is no correlation with Depature time
# also it appears that schedule elapse time and distance are highly correlated
corflight <- cor(flight[,c(1,2,3,5,8)])
corrplot(corflight, method = 'number',number.cex=0.6,title="Heat map",mar=c(0,0,1,0))
corrplot(corflight, method = "circle")

#Cancellations by month
# we can observe that their are higher cancellations in the first two quarters
#compared to 3rd and 4th quarters.
#Apr seems to have highest cancellations and Nov the lowest

ggplot(flight, aes(x = factor(Month), y = sum(Canceled))) + 
  geom_bar(stat = "identity")


## Used to check if grouping months would add accuracy to the model. But it ended up not helping. 
WOEstuffs2 = woe.binning(df = flight, 'Canceled', 'Month' )
WOEstuffs = as.data.frame(WOEstuffs2)
woe.binning.table(WOEstuffs2)
woe.binning.plot(WOEstuffs2)

## define the train and test sets
## as the data is imbalanced, let us sample equally from both categories

flightcanc <- flight[(flight$Canceled == 1),]
flightnoncanc <- flight[(flight$Canceled != 1),]
set.seed(10)  # <-- group 10


train <- rbind(flightcanc[(sample(1:nrow(flightcanc), 600)),] , flightnoncanc[(sample(1:nrow(flightnoncanc), 600)),])
testset<-setdiff(flight,train)



###########################################################################
#Null model
# The AUC observed is very less @0.49001
###########################################################################
m0 <- glm (Canceled ~ 1, data = train , family = binomial )
summary(m0)
pred0 <- predict(m0,testset,type =  "response")
# prob0 <- ifelse (pred0 > 0.16, 1,0)
# table(prob0, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred0)
AUCm0 <- ROC_func(dff, 1, 2, add_on = F, color = 1)
cat(paste0("Null Model  AUC is ", ": ", format(AUCm0, digits = 5), "\n"))
###########################################################################
# Full model
# The AUC observed is quite high @0.72416  but ARR delay might have to be removed
###########################################################################
mfull <- glm (Canceled ~ . , data = train , family = binomial )
predfull <- predict(mfull,testset,type =  "response")
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = predfull)
AUCmfull <- ROC_func(dff, 1, 2, add_on = T, color = 2)
cat(paste0("Model mfull  AUC is ", ": ", format(AUCmfull, digits = 5), "\n"))

###########################################################################

m1 <- glm (Canceled ~ .-ArrDelay , data = train , family = binomial )
pred1 <- predict(m1,testset,type =  "response")
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred1)
AUCm1 <- ROC_func(dff, 1, 2, add_on = T, color = 3)
cat(paste0("Model  AUC is ", ": ", format(AUCm1, digits = 5), "\n"))

###########################################################################
# we can observe that DepartureTime also is insignificant in the below model
#AUC is 0.67546
###########################################################################
m2 <- glm (Canceled ~ .-ArrDelay - DepDelay , data = train , family = binomial )
summary(m2)
pred2 <- predict(m2,testset,type =  "response")
prob2 <- ifelse (pred2 > 0.23, 1,0)
table(prob2, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred2)
AUCm2 <- ROC_func(dff, 1, 2, add_on = T, color = 4 )
cat(paste0("Model  AUC is ", ": ", format(AUCm2, digits = 5), "\n"))
###########################################################################
# Model 3 - AUC 0.66671
###########################################################################
m3 <- glm (Canceled ~ .-ArrDelay - DepDelay - DepartureTime  , data = train , family = binomial )
summary(m3)
pred3 <- predict(m3,testset,type =  "response")
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred3)
AUCm3 <- ROC_func(dff, 1, 2, add_on = T, color = 5)
cat(paste0("Model  AUC is ", ": ", format(AUCm3, digits = 5), "\n"))
###########################################################################
# We see that AUC declines if we remove unique carrier. So we can choose M3 as the best model
###########################################################################
m4 <- glm (Canceled ~ .-ArrDelay - DepDelay - DepartureTime - UniqueCarrier  , data = train , family = binomial )
summary(m4)
pred4 <- predict(m4,testset,type =  "response")
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred4)
AUCm4 <- ROC_func(dff, 1, 2, add_on = T, color = 6)
cat(paste0("Model  AUC is ", ": ", format(AUCm4, digits = 5), "\n"))
###########################################################################
#Randomforest Model  AUC is : 0.65626
trainrf<-train
trainrf$Canceled <- as.factor(trainrf$Canceled)
library(randomForest)
mrf <- randomForest (Canceled ~ .-ArrDelay - DepDelay -  DepartureTime ,data = trainrf  )
summary(mrf)
predrf <- predict(mrf,testset,type =  "response")
#prob2 <- ifelse (pred2 > 0.2, 1,0)
#table(prob2, testset$Canceled)
df <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                 m1 = predrf)
AUCrf <- ROC_func(df, 1, 2, add_on = T, color = 7)
cat(paste0("Model  AUC is ", ": ", format(AUCrf, digits = 5), "\n"))

###########################################################################
#Naive Bayes AUC is ~0.54696
library(naivebayes)
flightnb<-train
flightnb$Canceled <- as.factor(flightnb$Canceled)
modelnb1 <- naive_bayes(Canceled ~ Month + SchedElapsedTime + Distance+ UniqueCarrier , flightnb)
prednb1 <- predict(modelnb1,testset,type = "prob")
dffnb1 <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                     m1 = ifelse(prednb1[,1] > prednb1[,2] , prednb1[,1] ,prednb1[,2]))
AUCnb1 <- ROC_func(dffnb1, 1, 2, add_on = T, color = 8)
cat(paste0("Naive bayes Model   AUC is ", ": ", format(AUCnb1, digits = 5), "\n"))


# based on above anlaysis we choose Model 3, which excludes ARRDelay, DepDelay and DepartureTime
###########################################################################
#Averaging model performace over various training and test sets
###########################################################################
modelcoeff  <- data.frame(matrix(ncol = 6, nrow = 0))
for ( i in 1:100) {
  set.seed(i)
  # #define the train and test sets 
  train <- rbind(flightcanc[(sample(1:nrow(flightcanc), 600)),] , flightnoncanc[(sample(1:nrow(flightnoncanc), 600)),])
  testset<-setdiff(flight,train)
  #Build the model
  modelitr <- glm(Canceled ~ Month + SchedElapsedTime+ Distance+ UniqueCarrier, data = train ,family = binomial)
  modelcoeff[i,] <- coefficients(modelitr)
  cat(coefficients(modelitr))
  cat("\n")
}
colnames(modelcoeff) <- c("Intercept", "Month" , "SchedElapsedTime", "Distance", "UniqueCarrierDL","UniqueCarrierUA" )
summary(modelcoeff) # the average values are picked as func 1 coefficients

## testing the functions 
func1 <- function  (testinset) {
  testinset$prob <- 0
  for ( v in 1 : nrow(testinset))
  {
    uniquecarriercoeff =0;
    if (testinset[v,]$UniqueCarrier == 'DL')
    {uniquecarriercoeff = -0.7706  }
    if (testinset[v,]$UniqueCarrier == 'UA')
    {uniquecarriercoeff =-0.21274  }
    prob = exp(0.6540+
                 -0.10637 * as.numeric(testinset[v,]$Month)+
                 0.015055  * as.numeric(testinset[v,]$SchedElapsedTime)+
                 -0.002271  * as.numeric(testinset[v,]$Distance)+
                 uniquecarriercoeff)/(1+ exp(0.6540+
                                               -0.10637 * as.numeric(testinset[v,]$Month)+
                                               0.015055  * as.numeric(testinset[v,]$SchedElapsedTime)+
                                               -0.002271  * as.numeric(testinset[v,]$Distance)+
                                               uniquecarriercoeff))
    testinset[v,]$prob <-prob
    # cat(paste0("prob : " , prob, "cancellation value: ",testinset[v,]$Canceled , "\n"))
  }
  return (testinset$prob)
}

func2 <-function(testinset){
  score <- func1(testinset)
  label <- ifelse(score > 0.5055 , 1, 0)
  return(label)
}

######

sectest<-testset
probtest<-func1(sectest)

anaset <-  data.frame(sectest$Canceled,probtest)
pred<- ifelse(probtest > 0.5055,1,0) # adopting the threshold to predict cancellations with higer accuracy
table(sectest$Canceled, pred)
AUCtestanalysis_valmed <- ROC_func(anaset, 1, 2, add_on =F, color = 21)
cat(paste0("Model  AUC is ", ": ", format(AUCtestanalysis_valmed, digits = 5), "\n"))
