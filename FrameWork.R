# Preprocess data, Out put testSet.txt, trainSet.txt
setwd("D:\\Hackathon\\Hackathon")
library(caret)

trainData<-read.csv("train.csv")
testData<-read.csv("test.csv")

##### Drop target column
str(trainData)
Survived<-trainData[,2]
trainData<-trainData[-c(2)]

##### merge train data, test data
totalData<-rbind(trainData,testData)
str(totalData)

#### Drop some data columns having no predictive power and convert factor to int
x<-as.integer(totalData[,11])
y<-as.integer(totalData[,4])
totalData<-totalData[-c(1,3,4,8,10,11)]
totalData$Embarked<-x
totalData$Sex<-y

for(j in 1:ncol(totalData))
{
  for(i in 1:length(totalData[,j]))
  {
    if(is.na(totalData[i,j]))
      totalData[i,j]<-0
  }
}

###### preProcess
pp_zscore  <-  preProcess(totalData,  method	=	c("center",	"scale"))
totalData	<-	predict(pp_zscore,	totalData)

###### Drop near zero var column
nzv<-nearZeroVar(totalData)
if(length(nzv)!=0)
{
  totalData<-totalData[,-nzv]  
}

###### Drop highly correlation column
mCor<-cor(totalData)
vCor<- findCorrelation(mCor)
if(length(vCor)!=0)
{
  totalData<-totalData[,-vCor]  
}

###### Drop linear dependence column
# comboInfo<-findLinearCombos(totalData)
# if(length(comboInfo$remove)!=0)
# {
#   totalData<-totalData[,-comboInfo$remove]  
# }

###### Split data to trainSet, testSet and save to file
K<-length(Survived)
trainSet<-totalData[1:K,]
K<-K+1
testSet<-totalData[K:nrow(totalData),]

trainSet$OUTPUT<-Survived
#trainSet$OUTPUT<-factor(trainSet$OUTPUT)

library(randomForest)

train.RF <- randomForest(OUTPUT~.,data=trainSet)

fit.RF <- predict(train.RF,testSet)

write.table(fit.RF,file="res.txt",row.names=FALSE)







