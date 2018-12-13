############################################
## Predict PlantEF with Linear Regression ##
############################################

##Input data
train.data.frame <- TEST3
test.data.frame <- TEST4

##Perform Linear Regression
EFF.LR<-lm(BoilerEFF ~., data=train.data.frame)
summary(EFF.LR)

predicted.EFF.LR <- predict(EFF.LR, data=test.data.frame)
as.data.frame( cbind(predicted.EFF.LR, test.data.frame$BoilerEFF) )

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$BoilerEFF,type="l",pch=16,col="blue",
     ylab="BoilerEFF",xlab=NA,main="Linear Regression 모델",lwd=3)
lines(predicted.EFF.LR,col="red",lwd=2.5)

##Check error rate of Linear Regression
error.rate.predicted.EFF.LR<-(abs(predicted.EFF.LR
                                  -test.data.frame$BoilerEFF)
                              /test.data.frame$BoilerEFF)*100
mean.error.rate.predicted.EFF.LR<-mean(error.rate.predicted.EFF.LR)
mean.error.rate.predicted.EFF.LR


##############################
## Predict PlantEF with SVR ##
##############################

install.packages("e1071")
library(e1071)

##Perform SVR
EFF.SVR <- svm(BoilerEFF~., kernal="radial", data=train.data.frame,
               cost = 1)
summary(EFF.SVR)

predicted.EFF.SVR <- predict(EFF.SVR, data=test.data.frame)
as.data.frame( cbind(predicted.EFF.SVR, test.data.frame$BoilerEFF) )

##SVR prediction plot with test.data.frame
plot(test.data.frame$BoilerEFF,type="l",pch=16,col="blue",ylab="BoilerEFF",xlab=NA,main="SVR 모델",lwd=3)
lines(predicted.EFF.SVR,col="red",lwd=2.5)

##Check error rate of SVR
error.rate.predicted.EFF.SVR<-(abs(predicted.EFF.SVR
                                   -test.data.frame$BoilerEFF)
                               /test.data.frame$BoilerEFF)*100
mean.error.rate.predicted.EFF.SVR<-mean(error.rate.predicted.EFF.SVR)
mean.error.rate.predicted.EFF.SVR


##############################
## Predict PlantEF with H2O ##
##############################

install.packages("h2o")
install.packages("methods")
library(methods)
library(h2o)
h2o.init()

##Input data
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "BoilerEFF"
x <- setdiff(names(train.data.frame.h2o),y)

##Perform ANN
EFF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o)
EFF.h2o #결과보기

predicted.EFF.h2o <- h2o.predict(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o <- h2o.performance(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.EFF.h2o$predict, test.data.frame.h2o$BoilerEFF) )

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.EFF.h2o$predict))
test.BoilerEFF <- as.data.frame(h2o.asnumeric(test.data.frame.h2o$BoilerEFF))
test.BoilerEFF <- test.BoilerEFF[[1]]

##ANN prediction plot with test data
plot(test.BoilerEFF,type="l",pch=16,
     col="blue", ylab="BoilerEFF",xlab=NA,main="DeepLearning Model",lwd=3)
lines(predict.line.h2o, col="red",lwd=2)

##Check error rate of ANN
error.rate.predicted.EFF.h2o<-(abs(predicted.EFF.h2o
                                   -test.data.frame.h2o$BoilerEFF)
                               /test.data.frame.h2o$BoilerEFF)*100
mean.error.rate.predicted.EFF.h2o<-mean(error.rate.predicted.EFF.h2o)
mean.error.rate.predicted.EFF.h2o

