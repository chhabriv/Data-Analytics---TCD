#author: Viren Chhabria
#ID: 18301780
#Course: Data Analytics (CS7DS1)

#install.packages("readxl")
library(readxl)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("partykit")
library(partykit)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape")
library(reshape)
#install.packages("VIM")
library(VIM)
#install.packages("randomForest")
library(randomForest)
#path to save all the plots
path="H:/TCD/DataAnalytics/viren/plots/FinalAssignment/"
Data = read_excel("H:/TCD/DataAnalytics/viren/datasets/Project Data.xlsx")
str(Data)
attach(Data)
#removing the first ID column
Data = Data[, -1]
#selecting the columns that are factors
cols = c(1,2,10:16)
#converting required columns to factors - Group, Response, all Y's
Data[cols] = lapply(Data[cols], factor)

# ggplot(data = na.omit(Data), 
#        aes(x = X1,
#            y = Y1,
#            colour= Group)) +
#   geom_point() + xlim(0,250) + ggtitle("Relation between X1 and Y1 with respect to Group")
# 
# ggplot(data = na.omit(Data), 
#        aes(x = X2,
#            y = Y2,
#            colour= Group)) +
#   geom_point() + xlim(0,500) + ggtitle("Relation between X2 and Y2 with respect to Group")


#https://github.com/jevgenij-p/blog/blob/master/Missing%20Values%20Plot/MissingValues.R
plot_missing <- function(data, call,seg = 10, col = NULL) {
  
  columns <- colnames(data)
  ncols <- length(columns)
  nrows <- nrow(data)
  segments <- ifelse(seg > 0, seg, 1)
  
  missing_intensity <- matrix(0, ncols, (segments + 1))
  seg_size <- nrows / segments
  
  index <- seq(from = 0, to = nrows, by = seg_size)
  if (index[length(index)] < nrows) {
    index <- c(index, nrows)
  }
  
  index <- round(index)
  
  if (length(index) < (segments + 1)) {
    segments <- segments - 1
    missing_intensity <- matrix(0, ncols, (segments + 1))
  }
  
  for (i in 1:ncols) {
    index_i <- is.na(data[, columns[i]])
    missing_intensity[i, 1] <- sum(index_i) / nrows * 100
    for (j in 1:segments) {
      start_index <- index[j] + 1
      end_index <- index[j + 1]
      index_j <- is.na(data[start_index:end_index, columns[i]])
      missing_intensity[i, (j + 1)] <- sum(index_j) / (end_index - start_index + 1) * 100
    }
  }
  
  general_rate <- round(mean(missing_intensity[, 1]), 2)
  sort_index <- sort.int(-missing_intensity[, 1], index.return = T)$ix
  missing_intensity <- data.frame(missing_intensity)
  rownames(missing_intensity) <- columns
  colnames(missing_intensity) <- c('All', c(1:segments));
  missing_intensity <- missing_intensity[sort_index,]
  rnames <- factor(rownames(missing_intensity), levels = rev(rownames(missing_intensity)), ordered = F)
  
  missing_intensity <- cbind(Columns = rnames, missing_intensity)
  missvalues <- melt(missing_intensity, id = c('Columns'))
  
  new.palette <- colorRampPalette(c('royalblue4', 'seagreen3', 'yellow', 'orangered', 'black'))(100)
  
  if (!is.null(col))
    new.palette = col
  
  ggplot(missvalues, aes(x = variable, y = Columns, fill = value)) +
    geom_tile() +
    xlab('Segments') + 
    ggtitle(paste0('Missing Value Rate (', general_rate, '%)')) +
    scale_fill_gradientn(name = 'Missing\nvalue', colours = new.palette)
  ggsave(filename=paste("Missing Values",call,".jpg",sep=""),plot=last_plot(),
         device="jpeg",path=path)
  #,width = 4.55, height = 2.4)
}

#correlation heatmap
corr = function(Data,cols,exclude,call){
  if (exclude == 1) {
    cormat <- round(cor(Data[-cols],use="complete.obs"),2)
  }
  else
  {
    cormat <- round(cor(Data[cols],use="complete.obs"),2)
  }
  head(cormat)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  ggsave(filename=paste("Correlation Heatmap",call,".jpg",sep=""),plot=last_plot(),
         device="jpeg",path=path)
  #,width = 4.55, height = 2.4)
}

#Decision Tree
decision_tree = function(formula,Data,prune,title){
  DT<-rpart(formula, data=Data)
  jpeg(filename=paste(path,title,".jpg",sep=""),width = 1366, height = 720)
  plot(as.party(DT),main=title)
  dev.off()
  if (prune == 1) {
    DT$cptable
    opt=which.min(DT$cptable [,"xerror"])
    #pruning
    cp=DT$cptable [opt,"CP"]
    DT_pruned=prune(DT,cp = cp)
    jpeg(filename=paste(path,title,".jpg",sep=""),width = 1366, height = 720)
    plot(as.party(DT_pruned),main=paste("Pruned -",title,sep=" "))    
    dev.off()  
  }
}

#Random Forest
random_forest = function(formula, Data, title)
{
  RF=randomForest(formula,data=Data)
  #print(RF)
  importance(RF)
  jpeg(filename=paste(path,title,"Variable Importance Plot",".jpg",sep=""),width = 1366, height = 720)
  varImpPlot(RF,main="Variable Importance Plot")
  dev.off()
  jpeg(filename=paste(path,title,"Error Plot",".jpg",sep=""),width = 1366, height = 720)
  plot(RF,main="Error Plot")
  dev.off()
}

#function to build trees
build_trees = function(Data,call)
{
  #view missing Data
  plot_missing(Data,call)
  #correlation heatmap with complete observations - only X's
  corr(Data,cols,1,call)
  #All X
  formula=as.formula(Response~X1+X2+X3+X4+X5+X6+X7)
  title=paste("All X",call,sep=" ")
  decision_tree(formula,Data,1,title)
  #All X with Group
  formula=as.formula(Response~Group+X1+X2+X3+X4+X5+X6+X7)
  title=paste("All X plus Group",call,sep=" ")
  decision_tree(formula,Data,1,title)
  #All Y
  formula=as.formula(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7)
  title=paste("All Y",call,sep=" ")
  decision_tree(formula,Data,1,title)
  #All Y with Group
  formula=as.formula(Response~Group+Y1+Y2+Y3+Y4+Y5+Y6+Y7)
  title=paste("All Y plus group",call,sep=" ")
  decision_tree(formula,Data,1,title)
  #All
  formula=as.formula(Response~.)
  title=paste("All inputs",call,sep=" ")
  decision_tree(formula,Data,1,title)
  #Random Forest
  formula=as.formula(Response~.)
  title=paste("RF",call,sep=" ")
  random_forest(formula,na.omit(Data),title)
}

build_trees(Data,"- with missing data")

#impute missing data using kNN
summary(Data)
imputeData=kNN(Data,k=5)
summary(imputeData[1:16])

build_trees(imputeData[1:16], "- with kNN imputed Data")




