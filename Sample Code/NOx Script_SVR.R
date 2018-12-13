#################################################################
## Compare Linear Regression, SVR (x=NOX�߻���, y=���Ϸ� ȿ��) ##
#################################################################

memory.limit()
memory.limit(size=4000)
object.size(boiler9$NOX�߻���)

install.packages("e1071")
library(e1071)

##To see input dataset
data.frame<-head(boiler9,30)
print(data.frame)
plot(boiler9$NOX�߻���,boiler9$���Ϸ�ȿ��)

##Run linear regression
lm(formula=���Ϸ�ȿ�� ~NOX�߻���, data=boiler9)
lr1<-lm(formula=���Ϸ�ȿ�� ~NOX�߻���, data=boiler9)
summary(lr1)

##plot the Linear Regression line
plot(���Ϸ�ȿ��~NOX�߻���, data=boiler9, pch=16, col="blue",xlab="Nox�߻���",ylab="���Ϸ�ȿ��",main="Linear Regression Model")
abline(lr1,col="red",pch=16,lwd=8)

##Linear model accuracy(r.squared, MSE)
summary(lr1)$r.squared
summary(lr1)$adj.r.squared

pred.linearR<-predict(lr1,data=boiler9)

squaredError.lr1.reg<- (pred.linearR-boiler9$���Ϸ�ȿ��)^2
MSE.lr1.Regression <- mean(squaredError.lr1.reg)
MSE.lr1.Regression
#[1] 20.26687
rmse.lr1=sqrt(MSE.lr1.Regression)
rmse.lr1
#[1] 4.501874

##Run SVR
model1 <- svm(formula=���Ϸ�ȿ�� ~NOX�߻���, data=boiler9,type="nu-regression",kernel="radial")
summary(model1)

pred.SVR1<-predict(model1,data=boiler9)

##plot the SVR line
plot(���Ϸ�ȿ��~NOX�߻���,data=boiler9,pch=16,col="blue",xlab="NOx�߻���",ylab="���Ϸ�ȿ��",main="SVR Model")
points(boiler9$NOX�߻���,pred.SVR1,col="red",pch=17,cex=3)
#lines(boiler9$NOX�߻���,pred.SVR1,col="red",lwd=3)

##SVR model accuracy(r.squared, MSE)
squaredError.SVR1.reg<- (pred.SVR1-boiler9$���Ϸ�ȿ��)^2
MSE.SVR1.Regression <- mean(squaredError.SVR1.reg)
MSE.SVR1.Regression
#[1] 19.77514
rmse.SVR1=sqrt(MSE.SVR1.Regression)
rmse.SVR1
#[1] 4.446925

#################################################################
## Compare Linear Regression, SVR (x=BFG���Է�, y=���Ϸ� ȿ��) ##
#################################################################

##To see input dataset
plot(boiler9$BFG���Է�,boiler9$���Ϸ�ȿ��)

##Run linear regression
lm(formula=���Ϸ�ȿ�� ~BFG���Է�, data=boiler9)
lr2<-lm(formula=���Ϸ�ȿ�� ~BFG���Է�, data=boiler9)
summary(lr2)

##plot the Linear Regression line
plot(���Ϸ�ȿ��~BFG���Է�, data=boiler9, pch=16, col="blue",xlab="BFG���Է�",ylab="���Ϸ�ȿ��",main="Linear Regression Model")
abline(lr2,col="red",lwd=8)

##Linear model accuracy(r.squared, MSE)
summary(lr2)$r.squared
summary(lr2)$adj.r.squared

pred.linearR<-predict(lr2,data=boiler9)

squaredError.lr2.reg<- (pred.linearR-boiler9$���Ϸ�ȿ��)^2
MSE.lr2.Regression <- mean(squaredError.lr2.reg)
MSE.lr2.Regression
#[1] 17.98885
rmse.lr2=sqrt(MSE.lr2.Regression)
rmse.lr2
#[1] 4.241327

##Run SVR
model2 <- svm(formula=���Ϸ�ȿ�� ~BFG���Է�, data=boiler9,type="nu-regression",kernel="radial")
summary(model2)

pred.SVR2<-predict(model2,data=boiler9)

##plot the SVR line
plot(���Ϸ�ȿ��~BFG���Է�,data=boiler9,pch=16,col="blue",xlab="BFG���Է�",ylab="���Ϸ�ȿ��",main="SVR Model")
points(boiler9$BFG���Է�,pred.SVR2,col="red",pch=17,cex=2.5)
#lines(boiler9$BFG���Է�,pred.SVR2,col="red")

##SVR model accuracy(r.squared, MSE)
squaredError.SVR2.reg<- (pred.SVR2-boiler9$���Ϸ�ȿ��)^2
MSE.SVR2.Regression <- mean(squaredError.SVR2.reg)
MSE.SVR2.Regression
#[1] 12.28322
rmse.SVR2=sqrt(MSE.SVR2.Regression)
rmse.SVR2
#[1] 3.504742

##################################
## Run SVR with full variables) ##
##################################

model <- svm(���Ϸ�ȿ��~.,  kernal="radial", data=boiler9)
summary(model)

pred <- predict(model, boiler9)
table(pred, boiler9$���Ϸ�ȿ��)


##SVR model accuracy(rmse, MSE)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- boiler9$���Ϸ�ȿ�� - pred
svrPredictionRMSE<-rmse(error)
svrPredictionRMSE
#[1] 1.581338

##plot the SVR prediction line
plot(boiler9$���Ϸ�ȿ��,type="l",pch=16,col="blue",ylab="���Ϸ�ȿ��",main="SVR ��")
lines(pred,col="red")
#points(pdn,col="red",pch=17,cex=1)

############################################
## Run Linear Regression (full variables) ##
############################################

lm(formula=���Ϸ�ȿ��~., data=boiler9)
lrA<-lm(formula=���Ϸ�ȿ��~., data=boiler9)
summary(lrA)

pred.linearR<-predict(lrA,data=boiler9)

squaredError.lrA.reg<- (pred.linearR-boiler9$���Ϸ�ȿ��)^2
MSE.lrA.Regression <- mean(squaredError.lrA.reg)
MSE.lrA.Regression
#[1] 8.638585
rmse.lrA=sqrt(MSE.lrA.Regression)
rmse.lrA
#[1] 2.939147

##########################
## 3D Visualization SVR ##
##########################

install.packages("plot3D")
install.packages("plot3Drgl")
install.packages("rgl")
install.packages("lattice")
library("plot3D")
library("plot3Drgl")
library("rgl")
library("misc3d")
library("lattice")

data.frame<-boiler9_eng[c("NOX","CO","EFF")]
x<-data.frame$NOX
y<-data.frame$CO
z<-data.frame$EFF

##Input data 3D scatter plot
scatter3D(x,y,z,pch=16,cex=1,theta=20,phi=5,bty='g', col.panel="steelblue",
                  col.grid="darkblue", expand=0.6, 
                  main="data.frame",xlab="NOx",ylab="CO",
                  zlab="EFF",clab=c("EFF(%)"))
plotrgl()


##Perform SVR
model <- svm(EFF~.,  kernal="radial", data=data.frame)
summary(model)
pred<-predict(model,data=data.frame)

##Switch "z" values to SVR prediction values
z<-pred

##SVR hyperplane 3D scatter plot
scatter3D(x,y,z,pch=16,cex=1,theta=20,bty='g', col.panel="steelblue",
          col.grid="darkblue", expand=0.6, phi=5,
          main="data.frame",xlab="NOx",ylab="CO",
          zlab="EFF",clab=c("EFF(%)"))
plotrgl()

##################################
## SVM 3D Visualization Example ##
##################################

n    = 100
nnew = 50

# Simulate some data
set.seed(12345)
group = sample(2, n, replace=T)
dat   = data.frame(group=factor(group), matrix(rnorm(n*3, rep(group, each=3)), ncol=3, byrow=T))

# Fit SVM
fit = svm(group ~ ., data=dat)

# Plot original data
plot3d(dat[,-1], col=dat$group)

# Get decision values for a new data grid
newdat.list = lapply(dat[,-1], function(x) seq(min(x), max(x), len=nnew))
newdat      = expand.grid(newdat.list)
newdat.pred = predict(fit, newdata=newdat, decision.values=T)
newdat.dv   = attr(newdat.pred, 'decision.values')
newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))

# Fit/plot an isosurface to the decision boundary
contour3d(newdat.dv, level=0, x=newdat.list$X1, y=newdat.list$X2, z=newdat.list$X3, add=T)

####################################
