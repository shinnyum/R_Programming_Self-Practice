memory.limit()
memory.limit(size=4000)

#######################################
##Perform SVM with "Boiler9_PCA" data##
#######################################

install.packages("e1071")
install.packages("caret")
install.packages("ROCR")
library(e1071)
library(caret)
library(ROCR)


read_excel("")
x<-subset(boiler9_PCA, select=-EFGrade)
y<-as.factor(boiler9_PCA$EFGrade)

model<-svm(x, y, type="C-classification", kernel="radial")
svm.pred<-predict(model,x)

#Show confusionMatrix
confusionMatrixTable<-table(Predicted=svm.pred,Reference=boiler9_PCA$EFGrade)
confusionMatrix(confusionMatrixTable,positive='TRUE')
plot(cmdscale(dist(subset(boiler9_PCA, select=-EFGrade))), col=boiler9_PCA$EFGrade, pch=c("o","+")[1:150%in%model$index+1])

#Show ROC Curve
#model_ROC<-svm(x, y, type="C-classification", kernel="radial",probability=TRUE)
#pred.svm.Radial <- predict(model_ROC, boiler9_PCA[,-1],probability=TRUE)

#svm.pred_ROC<-prediction(attr(pred.svm.Radial,"probability")[,2],boiler9_PCA$EFGrade)
#perf<-performance(svm.pred,"tpr","fpr")
