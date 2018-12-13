install.packages("psych")
library(psych)

pairs(boiler9_finaldata[,2:13], col = as.numeric(boiler9_finaldata$EFGrade), pch = 21)
color.index<-as.factor(boiler9_finaldata$EFGrade)
pairs.panels(boiler9_finaldata[,2:13], scale=TRUE, font.labels=2, cex.labels=1, bg=c("red","orange","green","blue","black")[color.index], pch=23, cex=2.5)
