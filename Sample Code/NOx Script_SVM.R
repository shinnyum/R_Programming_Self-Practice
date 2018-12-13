memory.limit()
memory.limit(size=4000)
object.size(boiler9$NOX발생량)

###############
##SVM Example##
###############

install.packages("e1071")
library(e1071)

x<-subset(boiler9_class, select=보일러효율:LDG투입량)
y<-as.factor(boiler9_class$NOx_Class)

model<-svm(x, y, type="C-classification", kernel="radial")
print(model)
plot(cmdscale(dist(subset(boiler9_class, select=보일러효율:LDG투입량))), col=boiler9_class$NOx_Class+5, pch=c("o","+")[1:150%in%model$index+1])

#######################################
##Perform SVM with "Boiler9_PCA" data##
#######################################

install.packages("caret")
install.packages("ROCR")
library(caret)
library(ROCR)

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
