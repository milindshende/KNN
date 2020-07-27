library(caTools)

glass <- read.csv(file.choose()) # read csv file
View(glass)
dim(glass) # 214 x 10
class(glass) # data.frame

table(glass$Type) #table of diagonis 1=70,2=76,3=17,5=13,6=9,7=29
prop.table(table(glass$Type)) 
round(prop.table(table(glass$Type))*100,2) # 1=32.71,2=35.51,3=7.94,5=6.07,6=4.20,7=13.55
str(glass)
summary(glass) # check the min & max values (range) of each variable to decide on normalisation.


# Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#Apply the normalization function to dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm)) # not considered last column which is type/category of glass.
glass_n<- cbind(glass_n,Type=glass$Type)
summary(glass_n) # check the min & max values are between 0 & 1 otherthan 'Type'variable.

# split the date into Train & Test Data (70/30 ratio)
split<-sample.split(glass$Type,SplitRatio = 0.70)
table(split) # F=65 , T=149 Just to check the split ratio

glass_train<-subset(glass,split==TRUE)
glass_test<-subset(glass,split==FALSE)

glass_train1<-glass_train[,1:9]
glass_test1<-glass_test[,1:9]

#Get labels for training and test datasets
glass_train_labels <- glass_train[,10]
glass_test_labels <- glass_test[,10]

round(prop.table(table(glass_train_labels))*100,2) #1=32.89,2=35.57,3=8.05,5=6.04,6=4.03,7=13.42
round(prop.table(table(glass_test_labels))*100,2)#1=32.31,2=35.38,3=7.69,5=6.15,6=4.62,7=13.85

library("class")
library("caret")

#To identify optimum value of k, generally square root of total no of observations (149) which is 12.20 , so will try with k=12,13 then will check for optimal value of k
sqrt(NROW(glass_train_labels)) # 12.20

# Build a KNN model on taining dataset & test on test dataset
glass_knn12 <-  knn(train = glass_train1, test = glass_test1, cl = glass_train_labels, k=12)
glass_knn13 <-  knn(train = glass_train1, test = glass_test1, cl = glass_train_labels, k=13)

## Accuracy of above models
ACC_glass_knn12 <- 100 * sum(glass_test_labels == glass_knn12)/NROW(glass_test_labels)#k=12
ACC_glass_knn13 <- 100 * sum(glass_test_labels == glass_knn13)/NROW(glass_test_labels)#k=13
ACC_glass_knn12  # 66.15
ACC_glass_knn13  # 64.61

# Prediction against actual value in tabular form
table(glass_knn12 ,glass_test_labels) 
table(glass_knn13 ,glass_test_labels)

library("gmodels")
#create crosstable of predicted and actual
CrossTable(y=glass_test_labels,x=glass_knn12)
CrossTable(y=glass_test_labels,x=glass_knn13)

# Improve the performance of model

i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:15){ 
  glass_pred <-  knn(train = glass_train1, test = glass_test1, cl = glass_train_labels, k=i)
  k.optm[i] <- 100 * sum(glass_test_labels == glass_pred)/NROW(glass_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'%','\n')       # to print % accuracy 
}

# Maximum accuracy (78.46%) observed at k=1  

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value

###################################