#################################################
## Predict NOx emission with Linear Regression ##
#################################################

library(readxl)
CDQ_train <- read_excel("D:/work/dataset/CDQ.xlsx", sheet = "Sheet1")
CDQ_test <- read_excel("D:/work/dataset/CDQ.xlsx", sheet = "Sheet2")
View(CDQ_train)
View(CDQ_test)

##Input data
train.data.frame <- CDQ_train
test.data.frame <- CDQ_test

##Perform Linear Regression
EFF.LR<-lm(Y ~., data=train.data.frame)
summary(EFF.LR)

predicted.EFF.LR <- predict(EFF.LR, data=test.data.frame)
as.data.frame( cbind(predicted.EFF.LR, test.data.frame$Y) )

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$Y[0:300],type="l",pch=16,col="blue",
     ylab="Y",xlab=NA,main="Linear Regression 모델",lwd=5, ylim = c(60,110))
lines(predicted.EFF.LR[0:300],col="red",lwd=5)

##Check error rate of Linear Regression
error.rate.predicted.EFF.LR<-(abs(predicted.EFF.LR
                                  -test.data.frame$Y)
                              /test.data.frame$Y)*100
mean.error.rate.predicted.EFF.LR<-mean(error.rate.predicted.EFF.LR)
mean.error.rate.predicted.EFF.LR


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

y <- "Y"
x <- setdiff(names(train.data.frame.h2o),y)

##Perform ANN
EFF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, hidden = c(380,380,380), activation = "Maxout")
EFF.h2o #결과보기

predicted.EFF.h2o <- h2o.predict(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o <- h2o.performance(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.EFF.h2o$predict, test.data.frame.h2o$Y) )

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.EFF.h2o$predict))
test.BoilerEFF <- as.data.frame(as.numeric(test.data.frame.h2o$Y))
test.BoilerEFF <- test.BoilerEFF[[1]]

##ANN prediction plot with test data
plot(test.BoilerEFF[0:300],type="l",pch=16,
     col="blue", ylab="Y",xlab=NA,main="DeepLearning Model",lwd=5, ylim = c(60,110))
lines(predict.line.h2o, col="red",lwd=5)

##Check error rate of ANN
error.rate.predicted.EFF.h2o<-(abs(predicted.EFF.h2o
                                   -test.data.frame.h2o$Y)
                               /test.data.frame.h2o$Y)*100
mean.error.rate.predicted.EFF.h2o<-mean(error.rate.predicted.EFF.h2o)
mean.error.rate.predicted.EFF.h2o

