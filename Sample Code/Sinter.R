#################################################
## Predict NOx emission with Linear Regression ##
#################################################
install.packages("readxl")

library(readxl)
Sinter_train <- read_excel("D:/work/dataset/Sinter.xlsx", sheet = "train")
Sinter_test <- read_excel("D:/work/dataset/Sinter.xlsx", sheet = "test")
View(Sinter_train)
View(Sinter_test)

##Input data
train.data.frame <- Sinter_train
test.data.frame <- Sinter_test

##Perform Linear Regression
EFF.LR<-lm(Steam_Production ~., train.data.frame)
summary(EFF.LR)

predicted.EFF.LR <- predict(EFF.LR, test.data.frame)
as.data.frame( cbind(predicted.EFF.LR, test.data.frame$Steam_Production) )

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$Steam_Production,type="l",pch=16,col="blue",
     ylab="Steam_Production",xlab=NA,main="Linear Regression 모델",lwd=2)
lines(predicted.EFF.LR,col="red",lwd=2)

##Check error rate of Linear Regression
error.rate.predicted.EFF.LR<-(abs(predicted.EFF.LR - test.data.frame$Steam_Production)/test.data.frame$Steam_Production)*100
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

y <- "Steam_Production"
x <- setdiff(names(train.data.frame.h2o),y)

##Perform ANN
EFF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o)
EFF.h2o #결과보기

predicted.EFF.h2o <- h2o.predict(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o <- h2o.performance(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.EFF.h2o$predict, test.data.frame.h2o$Steam_Production) )

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.EFF.h2o$predict))
test.BoilerEFF <- as.data.frame(as.numeric(test.data.frame.h2o$Steam_Production))
test.BoilerEFF <- test.BoilerEFF[[1]]

##ANN prediction plot with test data
plot(test.BoilerEFF,type="l",pch=16,
     col="blue", ylab="Steam_Production",xlab=NA,main="DeepLearning Model",lwd=2)
lines(predict.line.h2o, col="red",lwd=2)

##Check error rate of ANN
error.rate.predicted.EFF.h2o<-(abs(predicted.EFF.h2o-test.data.frame.h2o$Steam_Production)/test.data.frame.h2o$Steam_Production)*100
mean.error.rate.predicted.EFF.h2o <- mean(error.rate.predicted.EFF.h2o)
mean.error.rate.predicted.EFF.h2o

