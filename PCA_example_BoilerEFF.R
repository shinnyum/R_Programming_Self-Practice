#################
## PCA Example ##
#################

install.packages("FactoMineR")
library(FactoMineR)

##Add dataset
data<-boiler9_full[c("BFGBurner",
                     "MakeUpWater",
                     "BFGFlow",
                     "NOx",
                     "O2",
                     "Urea",
                     "CO")]
##Plot the pairwise scatterplots
pairs(data)

##Perform the Principal Components Analysis (Case 1)
boiler9.pca1<-princomp(data,scores=TRUE,cor=TRUE)
summary(boiler9.pca1)

##Print the scree plot
plot(boiler9.pca1)

##Print the biplot
biplot(boiler9.pca1)

##Print the loadings
boiler9.pca1$loadings

##Print the scores
boiler9.pca1$scores

##Perform the Principal Components Analysis (Case 2)
boiler9.pca2<-PCA(data, scale.unit=TRUE, ncp=5, graph=T)
summary(boiler9.pca2)

plot(boiler9.pca2,cex=0.5,shadow=TRUE)
plot(boiler9.pca2,cex=0.5,shadow=TRUE, select="cos2 0.7", unselect=1)

dimdesc(boiler9.pca2, axes=c(1,2,3))

###################################
## Perform PCA with boiler9 data ##
###################################

##Perform the Principal Components Analysis (Case 2)
boiler9.pca3<-PCA(boiler9_3days_EFGrade_, quali.sup=1, scale.unit=TRUE,
                  ncp=5, graph=T)

summary(boiler9.pca3)

##Print the score plot divided by "EFGrade"
plot(boiler9.pca3,cex=0.5,shadow=TRUE,habillage=1)
