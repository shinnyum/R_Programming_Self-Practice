##########################################
## SVR hyperplane 3D Visualization code ##
##########################################

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
library("readxl")

CDQ_train <- read_excel("D:/work/dataset/CDQ.xlsx", sheet = "Sheet1")

#Input data(PCA data)
data.frame<-CDQ_train[c("PC1","PC2","High_Pressure_Steam")]
x<-data.frame$PC1
y<-data.frame$PC2
z<-data.frame$High_Pressure_Steam

#3D scatter plot of PCA data
scatter3D(x,y,z,pch=16,cex=1,theta=0,phi=5,bty='g', col.panel="steelblue",
          col.grid="darkblue", expand=0.6,xlab="PC1",ylab="PC2",
          zlab="High_P_Steam",clab=c("High_P_Steam"))
plotrgl()

#Perform SVR with PCA data
model <- svm(High_Pressure_Steam~.,  kernal="radial", data=data.frame)
summary(model)

#Predict with training dataset
pred<-predict(model,data=data.frame)

#Switch "z" values to SVR prediction values
z<-pred

#3D scatter plot of SVR model
scatter3D(x,y,z,pch=16,cex=1,theta=20,bty='g', col.panel="steelblue",
          col.grid="darkblue", expand=0.3, phi=20,
          main="SVR_hyperplane",xlab="PC1",ylab="PC2",
          zlab="High_P_Steam",clab=c("High_P_Steam"), ellipsoid = TRUE)
plotrgl()


#3D surface plot of SVR hyperplane
data.frame2<-data.frame(x,y,z)
scatter3d(z ~x + y, data=data.frame2, ylab="High_P_Steam", xlab="PC1", zlab="PC2", 
          fit=c("linear","smooth"),surface.col=c("black","red"), bg.col="white",axis.ticks=TRUE,
          axis.col=c("black","black","black"), surface.alpha=0.2,neg.res.col=NA,
          square.col="white", point.col="darkblue", text.col="black",grid.col="blue",
          residuals=FALSE, fill="FALSE", grid.lines=40, sphere.size=1.5)
