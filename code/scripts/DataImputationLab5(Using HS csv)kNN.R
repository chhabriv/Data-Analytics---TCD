library(readxl)
Data <- read.csv("code/DataAnalytics/HS.csv")
str(Data)
attach(Data)

#view missing data
summary(Data)

installed.packages("VIM")
library("VIM")

#impute using kNN
impute1=kNN(Data,variable="Genre",k=5)
summary(impute1)

#impute continuous values using kNN
impute2 = kNN(Data, variable=c("Audience..score..","Profitability"),k=6)
summary(impute2)

#impute integer variable
impute3=kNN(Data,variable="Rotten.Tomatoes..",k=6)
summary(impute3)
