#######################
#  Machine Learning Script file
#######################
#  libraries

library(caret)  # also loads lattice and ggplot2
library(rpart)
library(party)
library(rattle)
library(rpart.part)
library(bagRboostR)
library(plyr)
library(dplyr)
library(parallel)

##########################
trainingfile<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingfile<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
downloaded<- date()

traindata<-read.csv(trainingfile, header=TRUE)
testdata <-read.csv(testingfile, header=TRUE)

View(traindata)
write.csv(traindata,file="traindata.csv")
write.csv(testdata, file="testdata.csv")

#########################################
#    Start the EDA
#########################################
#### Get the colums that contain "acc"
xc<-names(traindata)
gc1<- grep("roll",xc)
gc2<- grep("pitch", xc)
gc3<- grep("yaw",xc)
gc4<- grep("acc",xc)
gc<-c(gc1,gc2,gc3,gc4,160)   # must include the classe column
trdata<-select(traindata,gc)
nvz<-nearZeroVar(trdata,saveMetrics=TRUE)    #find the near zero variables
nvz
gc<-c(gc1,gc2,gc3,gc4,160)   # must include the classe column
trdata<-select(traindata,gc)
View(trdata)
nc<-names(trdata)
nc1<-grep("min",nc)
nc2<-grep("avg",nc)
nc3<-grep("stddev", nc)
nc4<-grep("var_",nc)
nc5<-grep("kurtosis", nc)
nc6<-grep("skew", nc)
trdata1<-select(trdata, -c(nc1,nc2,nc3,nc4,nc5))
View(trdata1)
nc6<-grep("skewness", nc)
nc7<-grep("max", nc)
nc8<-grep("amplitude", nc)
nc0<-c(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8)
trdata1<-select(trdata,-nc0)
View(trdata1)
trdata2<-select(tradata1,-c(1,2))
trdata2<-select(trdata1,-c(1,2))


#############################################
> varImpPlot(rf)
> plot(rf)
> rf

Call:
      randomForest(formula = classe ~ ., data = trdata2, ntree = 500,      replace = TRUE, importance = TRUE)
Type of random forest: classification
Number of trees: 500
No. of variables tried at each split: 4

OOB estimate of  error rate: 0.77%
Confusion matrix:
      A    B    C    D    E class.error
A 5558    9    9    4    0 0.003942652
B   30 3746   17    1    3 0.013431657
C    1   18 3387   16    0 0.010227937
D    3    1   21 3189    2 0.008395522
E    0    5    6    6 3590 0.004713058


plot(x=1:500,y=pdata$A, xlab="Number of Trees",ylab="Probability Error Values", type="l")
lines(x=1:500,y=pdata$B,col="green")
lines(x=1:500,y=pdata$C,col="blue")
lines(x=1:500,y=pdata$D,col="red")
lines(x=1:500,y=pdata$E,col="grey")
lines(x=1:500,y=pdata$OOB,col="orange")









######################## 1st test
# model = Recursive Partitioning and Regression Trees
# type
#modFit <- train(classe~. , method="rpart",  data=vp1)
modFit <- train(classe~. , method="rpart",  data=trdata1)
pre1 <- predict(modFit,vp1)
print ( modFit$finalModel )
library(rattle)
fancyRpartPlot(modFit$finalModel)     # nice plot output

########################  2nd test
# model = randomForest
set.seed(1954)
#rf<-train(classe~.,data=vp,method="rf", prox=TRUE, importance=TRUE, )
rf<-randomForest(formula=classe~. ,data=trdata1, ntree=500,replace=TRUE,importance=TRUE )
rf$finalModel

########################   3rd test
library(bagRboostR)
bg<-bagging(classe~., data=vp, test=testdata, m = 5, ntree = 500, mtry = NULL, trace = T)
#Error in na.fail.default(list(classe = c(1L, 1L, 3L, 2L, 1L, 1L, 5L, 4L,  :
# missing values in object

########################  4th test
# model = bagging with SQL interface
#library(PivotalR)

######################## 5th test
# model = bagging
library(party)
predictors <- data.frame(vp[,-21])
outcome<- vp$classe
treebag<- bag(predictors,outcome,B=10,
            bagControl=bagControl(fit=ctreeBag$fit,
                                  predict=ctreeBag$pred,
                                  aggregate=ctreeBag$aggregate))

######################### 6th test
# model = bagFDA
library(caret)
#fda<- bagFDA(formula=classe ~ ., data=vp1, B=50, keepX=TRUE, na.action=na.omit)
fda<- bagFDA(formula=classe ~ ., data=trdata1, B=50, keepX=TRUE)



set.seed(1954); rf<-randomForest(formula=classe~. ,data=trdata1, ntree=500,replace=TRUE,importance=TRUE)
rf$finalModel
summary(rf)
rf$importance
plot(rf)



