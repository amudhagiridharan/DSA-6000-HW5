### Extra EDA 



library(dplyr)
library(tidyverse)
library(woeBinning)
library(corrplot)

# Creating correlation matrix for diamonds dataset
colnames(flight)
D <- cor(flight[,c(1,2,3,5,8)])

corrplot(D, method = "circle")

ggplot(flight, aes(x = factor(Month), y = sum(Canceled))) + 
  geom_bar(stat = "identity")

WOEstuffs2 = woe.binning(df = flight, 'Canceled', 'Month' )
WOEstuffs = as.data.frame(WOEstuffs2)
woe.binning.table(WOEstuffs2)
woe.binning.plot(WOEstuffs2)

# Making new month groupings using this 
flight$Month2 = case_when(
  flight$Month >= 0 &  flight$Month <= 4    ~ "Not Fall" ,
  flight$Month >= 5 &  flight$Month <= 8 ~ "Suummer",
  flight$Month == 9 ~ "September",
  flight$Month == 10 ~ "October",
  flight$Month == 11 ~ "November",
  flight$Month == 12 ~ "Decemeber",
  
  
  
)

