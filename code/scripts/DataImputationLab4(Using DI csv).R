library(readr)
Data <- read.csv("code/DataAnalytics/DI.csv",header=TRUE,sep=";")
str(Data)
attach(Data)
mean(Post_BP)
median(Post_BP)
#See Missing Data
Data$Post_BP[is.na(Data$Post_BP)]

#Find Mean and Median of non missing data
mean(Data$Post_BP[!is.na(Data$Post_BP)])
median(Data$Post_BP[!is.na(Data$Post_BP)])

#Use mean as replacement of missing data
Data1=Data
Data1$Post_BP[is.na(Data$Post_BP)] = mean(Data1$Post_BP[!is.na(Data$Post_BP)])
Data1$Post_BP

#Use median as replacement
Data2=Data
Data2$Post_BP[is.na(Data$Post_BP)] = median(Data2$Post_BP[!is.na(Data$Post_BP)])
Data2$Post_BP

#Remove column ID from the data
Data=Data[,-1]

#Correlation matrix
cor(Data)
#Remove missing data to see correlation better
cor(Data,use='complete.obs')
#Symbols to see correlation better
symnum(cor(Data,use='complete.obs'))

#New column such that its values are 0 when data for column 'u' is missing, 1 otherwise
Ind_Function=function(u)
{
  x=dim(length(u))
  x[which(is.na(u))]=0
  x[which(!is.na(u))]=1
  return(x)
}

Data$I = Ind_Function(Data$Post_BP)
Data$I

#regression model for post bp using pre bp as independent variable
Model=lm(Post_BP ~ Pre_BP)
summary(Model)
#Only intercept and slope
Model$coefficients
#intercept
coef(Model)["(Intercept)"] 
#slope
coef(Model)["Pre_BP"] 

Data$Post_BP

for(i in 1:nrow(Data))
{
  if(Data$I[i]==0)
  {
    Data$Post_BP[i]=coef(Model)["(Intercept)"] + coef(Model)["Pre_BP"] * Pre_BP[i]
  }
}
Data$Post_BP