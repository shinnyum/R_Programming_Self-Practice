#Read data from Excel worksheets
install.packages("readxl")
library(readxl)
STS_PI_Trainset <- read_excel("D:/work/dataset/STS_PI.xlsx", sheet = "train")
#View(STS_PI_Trainset)
STS_PI_Testset <- read_excel("D:/work/dataset/STS_PI.xlsx", sheet = "test")
#View(STS_PI_Testset)

# df.iris <- data.frame(iris, stringsAsFactors = FALSE)
# 
# for (i in 1:nrow(df.iris)){
#   if(df.iris$Species[i] == "setosa") {
#     df.iris$Numeric.Species[i] <- 1
#   }
#   else if(df.iris$Species[i] == "versicolor"){
#     df.iris$Numeric.Species[i] <- 2
#   }
#   else if(df.iris$Species[i] == "virginica"){
#     df.iris$Numeric.Species[i] <- 3
#   }
#   df.iris$index[i] <- i
# }
# 
# # View(df.iris)
# 
# iris.train <- df.iris[sample(nrow(df.iris),100),]
# iris.test <- df.iris[!df.iris$index %in% iris.train$index, ]
# iris.test <- iris.test[sample(nrow(iris.test)),]
# 
# iris.train <- subset(iris.train, select = -index)
# iris.test <- subset(iris.test, select = -index)
# 
# View(iris.train)
# View(iris.test)

####################################################
## Predict Numeric.Species with Linear Regression ##
####################################################

##Input data
train.data.frame <- STS_PI_Trainset
test.data.frame <- STS_PI_Testset

train.data.frame <- subset(train.data.frame, select = c(-Date,-Material_Num,-Steel_Grade,-Mill_Class))
test.data.frame <- subset(test.data.frame, select = c(-Date,-Material_Num,-Steel_Grade,-Mill_Class))

train.data.frame <- as.data.frame(train.data.frame, stringsAsFactors = TRUE)
test.data.frame <- as.data.frame(test.data.frame, stringsAsFactors = TRUE)

##Perform Linear Regression
EFF.LR<-lm(Ton_per_Hour ~., data=train.data.frame)
summary(EFF.LR)

predicted.EFF.LR <- predict(EFF.LR, data=test.data.frame)
as.data.frame( cbind(predicted.EFF.LR, test.data.frame$Numeric.Species) )

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$Ton_per_Hour, type="l", pch=16, col="blue", 
     ylab="Ton_per_Hour",xlab=NA,main="Linear Regression 모델",lwd=3)
lines(predicted.EFF.LR,col="red",lwd=2.5)

##Check error rate of Linear Regression
error.rate.predicted.EFF.LR<-(abs(predicted.EFF.LR
                                  -test.data.frame$Ton_per_Hour)
                              /test.data.frame$Ton_per_Hour)*100
mean.error.rate.predicted.EFF.LR<-mean(error.rate.predicted.EFF.LR)
mean.error.rate.predicted.EFF.LR


######################################
## Predict Numeric.Species with SVR ##
######################################

install.packages("e1071")
library(e1071)

##Perform SVR
EFF.SVR <- svm(Ton_per_Hour~., kernal="radial", data=train.data.frame,
               cost = 1)
summary(EFF.SVR)

predicted.EFF.SVR <- predict(EFF.SVR, data=test.data.frame)
as.data.frame( cbind(predicted.EFF.SVR, test.data.frame$Ton_per_Hour) )

##SVR prediction plot with test.data.frame
plot(test.data.frame$Ton_per_Hour,type="l",pch=16,col="blue",ylab="Ton_per_Hour",xlab=NA,main="SVR 모델",lwd=3)
lines(predicted.EFF.SVR,col="red",lwd=2.5)

##Check error rate of SVR
error.rate.predicted.EFF.SVR<-(abs(predicted.EFF.SVR
                                   -test.data.frame$Ton_per_Hour)
                               /test.data.frame$Ton_per_Hour)*100
mean.error.rate.predicted.EFF.SVR<-mean(error.rate.predicted.EFF.SVR)
mean.error.rate.predicted.EFF.SVR


######################################
## Predict Numeric.Species with H2O ##
######################################

install.packages("h2o")
install.packages("methods")
library(methods)
library(h2o)
h2o.init()

##Input data
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "Ton_per_Hour"
x <- setdiff(names(train.data.frame.h2o),y)

##Perform ANN
EFF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Maxout", hidden = c(10))
EFF.h2o #결과보기

predicted.EFF.h2o <- h2o.predict(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o <- h2o.performance(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.EFF.h2o$predict, test.data.frame.h2o$Ton_per_Hour) )

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.EFF.h2o$predict))
test.Ton_per_Hour <- as.data.frame(as.numeric(test.data.frame.h2o$Ton_per_Hour))
test.Ton_per_Hour <- test.Ton_per_Hour[[1]]

##ANN prediction plot with test data
plot(test.Ton_per_Hour,type="l",pch=16,
     col="blue", ylab="Ton_per_Hour",xlab=NA,main="DeepLearning Model",lwd=3)
lines(predict.line.h2o, col="red",lwd=2)

##Check error rate of ANN
error.rate.predicted.EFF.h2o<-(abs(predicted.EFF.h2o
                                   -test.data.frame.h2o$Ton_per_Hour)
                               /test.data.frame.h2o$Ton_per_Hour)*100
mean.error.rate.predicted.EFF.h2o<-mean(error.rate.predicted.EFF.h2o)
mean.error.rate.predicted.EFF.h2o
