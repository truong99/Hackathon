setwd("C:\\Users\\admin\\Desktop")

install.packages("dplyr")
install.packages("randomForest")

# requiring library
library(dplyr)

# get training data
train <- read.csv("Data.csv")
str(train)

# get rid of unwanted features
train$ï..ID <- NULL
train <- train[, -c(13:151)]

# check if there is any empty data
sum(is.na(train))

# convert integer data to numeric data
dmy <- dummyVars(" ~ .", data = train,fullRank = T)
train <- data.frame(predict(dmy, newdata = train))

# convert churn's data type to factor type
train$churn <- as.factor(train$churn)
tail(train, 3)
View(train)

# split data 75% / 25%
index <- createDataPartition(train$churn, p=0.75, list=FALSE)
trainSet <- train[ index,]
testSet <- train[-index,]


library(randomForest)

rfClass <- randomForest(churn ~.,data = trainSet)
resultClass <- predict(rfClass, testSet, type = "class")
#table(testSet$churn, resultClass)
confusionMatrix(testSet$churn, resultClass)