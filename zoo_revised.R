zoo <- read.csv(file.choose()) # read csv file
View(zoo)
dim(zoo) # 101 x 18
class(zoo) # data.frame
zoo<-zoo[-1] # delete first column


# KNN is influenced by majority class & is biased method towards majority class.
# so we are checking the proportion of the class.
table(zoo$type) #table of diagonis 1=41,2=20,3=5,4=13,5=4,6=8,7=10
prop.table(table(zoo$type))
round(prop.table(table(zoo$type))*100,2)
str(zoo)
summary(zoo) # check the min & max values (range) of each variable to decide on normalisation.

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#Apply the normalization function to zoo dataset
zoo_n <- as.data.frame(lapply(zoo[1:16], norm)) # not considered last column which is type/category of animal.
View(zoo_n)
summary(zoo_n) # check the min & max values are between 0 & 1

#create training and test datasets
zoo_train <- zoo_n[1:80,] # 80% of total observations
zoo_test <- zoo_n[81:101,] # 20% of total observations

#Get labels for training and test datasets
zoo_train_labels <- zoo[1:80,17]
zoo_test_labels <- zoo[81:101,17]

round(prop.table(table(zoo_train_labels))*100,2)# 1=45,2=20,3=2.50,4=12.5,5=3.75,6=7.50,7=8.75
round(prop.table(table(zoo_test_labels))*100,2)# 1=23.81,2=19.05,3=14.29,4=14.29,5=4.76,6=9.52,7=14.29

library("class")
library("caret")

#To identify optimum value of k, generally square root of total no of observations (80) which is 8.94 is taken, so will try with 8,9 then will check for optimal value of k
sqrt(NROW(zoo_train_labels)) # 8.94

# Build a KNN model on taining dataset & test on test dataset
zoo_knn8 <-  knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=8)
zoo_knn9 <-  knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=9)

## Accuracy of above models
ACC_zoo_knn8 <- 100 * sum(zoo_test_labels == zoo_knn8)/NROW(zoo_test_labels)#k=8 
ACC_zoo_knn9 <- 100 * sum(zoo_test_labels == zoo_knn9)/NROW(zoo_test_labels)#k=9
ACC_zoo_knn8  # 76.19
ACC_zoo_knn9  # 71.42

# Prediction against actual value in tabular form
table(zoo_knn8 ,zoo_test_labels) 
table(zoo_knn9 ,zoo_test_labels)

library("gmodels")
#create crosstable of predicted and actual
CrossTable(y=zoo_test_labels,x=zoo_knn8)
CrossTable(y=zoo_test_labels,x=zoo_knn9)

# Improve the performance of model

i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:15){ 
  zoo_pred <-  knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  k.optm[i] <- 100 * sum(zoo_test_labels == zoo_pred)/NROW(zoo_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'%','\n')       # to print % accuracy 
}

# Maximum accuracy (85.714%) observed at k=2  

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value

###################################