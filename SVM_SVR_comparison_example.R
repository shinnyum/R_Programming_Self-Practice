#################################
## Perform SVM with PCA Scores ##
#################################

install.packages("e1071")
install.packages("ggplot2")
library(e1071)
library(ggplot2)

#Plot the PCA Scores(to see dataset)
plot(boiler9_SVR_Scores_$PC1,boiler9_SVR_Scores_$PC2)

x<-boiler9_SVR_Scores_[c("PC1","PC2")]
y<-as.factor(boiler9_SVR_Scores_$EFGrade)
df<-data.frame(cbind(x[],y[]))

#Perform SVM with PCA Scores
svm.model<-svm(y ~ PC2+PC1, data=df, type="C-classification", kernel="radial")
plot(svm.model, ylim=rev(range(-4,6)),data=df)

#################################
## Perform SVR with PCA Scores ##
#################################

#(X=PC1, Y=BoilerEFF)############
PC1.model <- svm(BoilerEFF~PC1, data=boiler9_SVR_Scores_,type="nu-regression",kernel="radial")
summary(PC1.model)

#Perform prediction with training data
pred.SVR1<-predict(PC1.model,data=subset(boiler9_SVR_Scores_,select=-EFGrade))

#SVR visualization
plot(BoilerEFF~PC1,data=boiler9_SVR_Scores_,pch=16,col="blue",xlab="PC1",ylab="BoilerEFF",main="SVR Model")
points(boiler9_SVR_Scores_$PC1,pred.SVR1,col="red",pch=17,cex=3)
#lines(boiler9$NOX발생량,pred.SVR1,col="red",lwd=3)

#(X=PC2, Y=BoilerEFF)############
PC2.model <- svm(BoilerEFF~PC2, data=boiler9_SVR_Scores_,type="nu-regression",kernel="radial")
summary(PC2.model)

#Perform prediction with training data
pred.SVR2<-predict(PC2.model,data=subset(boiler9_SVR_Scores_,select=-EFGrade))

#SVR visualization
plot(BoilerEFF~PC2,data=boiler9_SVR_Scores_,pch=16,col="blue",xlab="PC2",ylab="BoilerEFF",main="SVR Model")
points(boiler9_SVR_Scores_$PC2,pred.SVR2,col="red",pch=17,cex=3)

#(X=PC1,PC2, Y=BoilerEFF)########
install.packages("e1071")
install.packages("plot3D")
install.packages("plot3Drgl")
install.packages("rgl")
install.packages("lattice")
install.packages("car")
library("e1071")
library("plot3D")
library("plot3Drgl")
library("rgl")
library("misc3d")
library("lattice")
library("car")

#Input data(PCA data)
data.frame<-boiler9_SVR_3daysScores_[c("PC1","PC2","BoilerEFF")]
x<-data.frame$PC1
y<-data.frame$PC2
z<-data.frame$BoilerEFF

#3D scatter plot of PCA data
scatter3D(x,y,z,pch=16,cex=1,theta=20,phi=5,bty='g', col.panel="steelblue",
          col.grid="darkblue", expand=0.6, 
          main="data.frame",xlab="PC1",ylab="PC2",
          zlab="BoilerEFF",clab=c("EFF(%)"))
plotrgl()

#Perform SVR with PCA data
model <- svm(BoilerEFF~.,  kernal="radial", data=data.frame)
summary(model)

#Predict with training dataset
pred<-predict(model,data=data.frame)

#Switch "z" values to SVR prediction values
z<-pred

#3D scatter plot of SVR model
scatter3D(x,y,z,pch=16,cex=1,theta=20,bty='g', col.panel="steelblue",
          col.grid="darkblue", expand=0.3, phi=20,
          main="SVR_hyperplane",xlab="PC1",ylab="PC2",
          zlab="BoilerEFF",clab=c("EFF(%)"), ellipsoid = TRUE)
plotrgl()


#3D surface plot of SVR hyperplane
data.frame2<-data.frame(x,y,z)
scatter3d(z ~x + y, data=data.frame2, ylab="BoilerEFF", xlab="PC1", zlab="PC2", 
          fit=c("linear","smooth"),surface.col=c("black","red"), bg.col="white",axis.ticks=TRUE,
          axis.col=c("black","black","black"), surface.alpha=0.2,neg.res.col=NA,
          square.col="white", point.col="darkblue", text.col="black",grid.col="blue",
          residuals=FALSE, fill="TRUE", grid.lines=40, sphere.size=1.5)




######################################################
## Comparing error rate with Linear Regression, SVR ##
######################################################

data.frame<-boiler9_finaldata_h2o

##Split dataset with train(80%), test(20%) data
nbrow=nrow(data.frame)
ntrain <- round(nbrow*0.8)
tindex <- sample(nbrow,ntrain) # indices of training samples
train <- data.frame[tindex,]
test <- data.frame[-tindex,]

##Perform Linear Regression
lr1<-lm(BoilerEFF ~., data=train)
summary(lr1)

test.predict.LR <- predict(lr1, data=test)
as.data.frame( cbind(test.predict.LR, test$BoilerEFF) )

##Linear Regression prediction plot with test data
##########################################################################
plot(test$BoilerEFF,type="l",pch=16,col="blue",ylab="BoilerEFF",xlab=NA,main="Linear Regression 모델",lwd=3)
lines(test.predict.LR,col="green",lwd=2.5)
##########################################################################

##Check error rate of Linear Regression
test.predict <- predict(lr1, data=test)
train.predict<- predict(lr1, train)
percent.test<-(abs(test.predict-test$BoilerEFF)/test$BoilerEFF)*100
percent.train<-(abs(train.predict-train$BoilerEFF)/train$BoilerEFF)*100
mean.percent.test<-mean(percent.test)
mean.percent.train<-mean(percent.train)
mean.percent.test
mean.percent.train

##Perform SVR
model <- svm(BoilerEFF~., kernal="radial", data=train, cost = 1)

test.predict.SVR <- predict(model, data=test)

##SVR prediction plot with test data
##########################################################################
plot(test$BoilerEFF,type="l",pch=16,col="blue",ylab="BoilerEFF",xlab=NA,main="SVR 모델",lwd=3)
lines(test.predict.SVR,col="red",lwd=2.5)
##########################################################################

##Check error rate of SVR
test.predict <- predict(model, data=test)
train.predict<- predict(model, train)
percent.test<-(abs(test.predict-test$BoilerEFF)/test$BoilerEFF)*100
percent.train<-(abs(train.predict-train$BoilerEFF)/train$BoilerEFF)*100
mean.percent.test<-mean(percent.test)
mean.percent.train<-mean(percent.train)
mean.percent.test
mean.percent.train