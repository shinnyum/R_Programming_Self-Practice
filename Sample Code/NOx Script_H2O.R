#######################
## H2O Example(iris) ##
#######################

install.packages("h2o")
library(h2o)
library(xlsx)
h2o.init()

data <- read_excel("D:/work/dataset/DESH_OUT_TEMP", sheet = "Sheet1")
data <- as.h2o(data)

# X, Y ���� ����
y <- "class"
x <- setdiff(names(data), y)

# train, test set ���� 
parts <- h2o.splitFrame(data, 0.8)
train <- parts[[1]]
test <- parts[[2]]

# h2o deep learning �Լ� ���� �� performance ��� ����
m <- h2o.deeplearning(x, y, train)
m #�������
p <- h2o.predict(m, test)
perf <- h2o.performance(m,test)
perf #�������

# ������ ������ �Բ� �����ֱ�
as.data.frame( h2o.cbind(p$predict, test$class) )

###################################
## Predict NOx emission with H2O ##
###################################

install.packages("h2o")
install.packages("methods")
library(methods)
library(h2o)
h2o.init()

data<- as.h2o(boiler9_finaldata_h2o)

# X, Y ���� ����
y <- "BoilerEFF"
x <- setdiff(names(data),y)

# train, test set ���� 
parts <- h2o.splitFrame(data, ratios=(0.8), seed=30000)
train <- parts[[1]]
test <- parts[[2]]

# h2o deep learning �Լ� ���� �� performance ��� ����
m <- h2o.deeplearning(x, y, train)
m #�������
#MSE:  7.198035
#RMSE:  2.682915

p <- h2o.predict(m, test)
perf <- h2o.performance(m,test)
perf #�������
#MSE:  8.688118
#RMSE:  2.947561

# ������ ������ �Բ� �����ֱ�
as.data.frame( h2o.cbind(p$predict, test$BoilerEFF) )

# ������ ������ �� �׷��� �׸���
EFFpredict <- h2o.predict(m, test)
predict.line <- as.data.frame(h2o.cbind(EFFpredict$predict))
test.BoilerEFF<-as.data.frame(h2o.asnumeric(test$BoilerEFF))
BoilerEFF<-test.BoilerEFF[[1]]

##ANN prediction plot with test data
###########################################################################
plot(BoilerEFF,type="l",pch=16,
     col="blue", ylab="BoilerEFF",xlab=NA,main="DeepLearning Model",lwd=3)
lines(predict.line,col="black",lwd=2)
###########################################################################
#points(predict.line,col="red",pch=17,cex=1)

##Check error rate
test.predict <- h2o.predict(m, test)
train.predict<- h2o.predict(m, train)
percent.test<-(abs(test.predict-test$BoilerEFF)/test$BoilerEFF)*100
percent.train<-(abs(train.predict-train$BoilerEFF)/train$BoilerEFF)*100
mean.percent.test<-mean(percent.test)
mean.percent.train<-mean(percent.train)
mean.percent.test
mean.percent.train