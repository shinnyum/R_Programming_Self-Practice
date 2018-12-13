install.packages("psych")
library(psych)
library(readxl)

CDQ_train <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)1.xlsx", sheet = "train")

color.index<-as.factor(CDQ_train$Grade)
pairs.panels(CDQ_train[,2:13], scale=FALSE, font.labels=2, cex.labels=1, bg=c("red","orange","green","blue","black")[color.index], pch=23, cex=2.5)
