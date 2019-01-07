library(readxl)
library(rpart)
library(rpart.plot)
library(partykit)
library("VIM")
#install.packages("arm")
library(arm)
library(randomForest)
Data <- read.csv("~/code/DataAnalytics/siswave3v4impute3.csv")
str(Data)
attach(Data)
#Get all columns with no missing data
cols=c(names(which(sapply(Data, function(x) !any(is.na(x))))))
cols=append(cols,"rearn")
DataNew=Data[cols]
#Data2=sapply(DataNew,function(x) {as.numeric(levels(x))[x]})
# DT_Model2<-rpart(rearn~., data=DataNew) 
# plot(as.party(DT_Model2)) 
# print(DT_Model2) 


# Ind_Function=function(u)
# {
#   x=dim(length(u))
#   x[which(!is.na(u)) && u==0]=0
#   x[which(is.na(u))]=NA
#   x[which(!is.na(u)) && u>0]=1
#   return(x)
# }

#DataNew$I = Ind_Function(Data$rearn)
DataNew$I = I(Data$rearn)
DataNew$I=as.numeric(ifelse(DataNew$I>0,1,DataNew$I))

DataNew=na.omit(DataNew)
OnlyNum <- unlist(lapply(DataNew, is.numeric))  
toAdd=c(names(DataNew[ , OnlyNum]))
toAdd=append(toAdd,"fproj")
DataNew=DataNew[toAdd]
#attach(DataNew)

topcode <- function (a, top){
  return (ifelse (a>top, top, a))
}

impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

rearn.top=topcode(rearn,100000)
Model=glm(I(rearn>0) ~ zprearn+ztotinc+n33+m08+q3,
          family=binomial(link='logit'),data=Data)
#summary(Model)
#Only intercept and slope
print("Logistic Regression Coefficients: ")
Model$coefficients

lm.ifpos.sqrt <- lm (I(sqrt(rearn.top)) ~ zprearn+ztotinc+n33+m08+q3,data=Data,subset=Data$rearn>0) 
#summary(lm.ifpos.sqrt)
#Only intercept and slope
print("Linear Regression Coefficients: ")
lm.ifpos.sqrt$coefficients
#intercept
coef(lm.ifpos.sqrt)["(Intercept)"] 

n=nrow(Data)
pred.sign <- rbinom (n, 1, predict (Model, Data, type="response"))
pred.pos.sqrt <- rnorm (n, predict (lm.ifpos.sqrt, Data),sigma.hat(lm.ifpos.sqrt))

pred.pos <- topcode (pred.pos.sqrt^2, 100000)
earnings.imp <- impute (Data$rearn, pred.sign*pred.pos)
#View(earnings.imp)
print("Before Imputation:")
print("Mean:")
mean(na.omit(Data$rearn))
print("Standard Deviation")
sd(na.omit(Data$rearn))

print("After Imputation:")
print("Mean:")
mean(earnings.imp)
print("Standard Deviation")
sd(earnings.imp)

#summary(Model)

#install.packages("randomForest")
RF=randomForest(rearn~.,data=DataNew)
print(RF)
importance(RF)
varImpPlot(RF)
plot(RF)

