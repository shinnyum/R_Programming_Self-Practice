#################################################
## Predict NOx emission with Linear Regression ##
#################################################

##Input data
train.data.frame <- boiler9_finaldata_202122_NOx
test.data.frame <- boiler9_finaldata_232425_eraseOutlier_NOx

##Perform Linear Regression
NOx.LR<-lm(NOx ~., data=train.data.frame)
summary(NOx.LR)

predicted.NOx.LR <- predict(NOx.LR, data=test.data.frame)
as.data.frame( cbind(predicted.NOx.LR, test.data.frame$BoilerEFF) )

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$NOx,type="l",pch=16,col="blue",
     ylab="NOx",xlab=NA,main="Linear Regression 모델",lwd=3)
lines(predicted.NOx.LR,col="red",lwd=2.5)

##Check error rate of Linear Regression
error.rate.predicted.NOx.LR<-(abs(predicted.NOx.LR
                                  -test.data.frame$NOx)
                              /test.data.frame$NOx)*100
mean.error.rate.predicted.NOx.LR<-mean(error.rate.predicted.NOx.LR)
mean.error.rate.predicted.NOx.LR


###################################
## Predict NOx emission with SVR ##
###################################

install.packages("e1071")
library(e1071)

##Perform SVR
NOx.SVR <- svm(NOx~., kernal="radial", data=train.data.frame,
               cost = 0.05)
summary(NOx.SVR)

predicted.NOx.SVR <- predict(NOx.SVR, data=test.data.frame)
as.data.frame( cbind(predicted.NOx.SVR, test.data.frame$NOx) )

##SVR prediction plot with test.data.frame
plot(test.data.frame$NOx,type="l",pch=16,col="blue",ylab="NOx",xlab=NA,main="SVR 모델",lwd=3)
lines(predicted.NOx.SVR,col="red",lwd=2.5)

##Check error rate of SVR
error.rate.predicted.NOx.SVR<-(abs(predicted.NOx.SVR
                                   -test.data.frame$NOx)
                               /test.data.frame$NOx)*100
mean.error.rate.predicted.NOx.SVR<-mean(error.rate.predicted.NOx.SVR)
mean.error.rate.predicted.NOx.SVR


###################################
## Predict NOx emission with H2O ##
###################################

install.packages("h2o")
install.packages("methods")
library(methods)
library(h2o)
h2o.init()

##Input data
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "NOx"
x <- setdiff(names(train.data.frame.h2o),y)

##Perform ANN
NOx.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o)
NOx.h2o #결과보기

predicted.NOx.h2o <- h2o.predict(NOx.h2o, test.data.frame.h2o)
performance.NOx.h2o <- h2o.performance(NOx.h2o, test.data.frame.h2o)
performance.NOx.h2o #결과보기
as.data.frame( h2o.cbind(predicted.NOx.h2o$predict, test.data.frame.h2o$NOx) )

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.NOx.h2o$predict))
test.NOx <- as.data.frame(h2o.asnumeric(test.data.frame.h2o$NOx))
test.NOx <- test.NOx[[1]]

##ANN prediction plot with test data
plot(test.NOx,type="l",pch=16,
     col="blue", ylab="NOx",xlab=NA,main="DeepLearning Model",lwd=3)
lines(predict.line.h2o, col="red",lwd=2)

##Check error rate of ANN
error.rate.predicted.NOx.h2o<-(abs(predicted.NOx.h2o
                                   -test.data.frame.h2o$NOx)
                               /test.data.frame.h2o$NOx)*100
mean.error.rate.predicted.NOx.h2o<-mean(error.rate.predicted.NOx.h2o)
mean.error.rate.predicted.NOx.h2o

