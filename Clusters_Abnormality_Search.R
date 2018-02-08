#=======================================================================
# 분석개요
#1. 이상군집 탐색
#2. 데이터 구조 - 5개 변수, 6000개 → 20개 파생변수 312개  
# 사용자 정의함수 Loading
## MKS Function
#source("RSC\\(논문용)151129_MKS_function.R")
#source("RSC\\(논문용)160318_mahal_fn.R")
#=======================================================================
install.packages("ggplot2")
install.packages("Hmisc")

library(MASS)
library(Hmisc)
library(cluster)
library(ggplot2)
#=======================================================================
# 작업 경로 설정
setwd("D:\\작업방\\Rproject\\DAT")
#=======================================================================
#=======================================================================
# 사용자 정의함수 Loading
## MKS Function
source("RSC\\(논문용)151129_MKS_function.R")
source("RSC\\(논문용) 160318_mahal_fn.R")
#=======================================================================
# 분석용 데이터 불러오기
#=======================================================================
data_df <- read.table("clipboard", header = T, sep = "\t", stringsAsFactor = F)
write.csv(data_df, "DAT\\160326_anal_df_V20_ox.csv", row.names = F)
anal_df <- read.csv("DAT\\160326_anal_df_V20_ox.csv", header = TRUE, stringsAsFactors = FALSE)

anal_df <- iris[,c(1:4)]
anal_df_c <- iris


par(mfrow = c(2, 2))
hist(anal_df$Sepal.Length, breaks = 15, freq = F, xlab = 'Sepal.Length"', ylim = c(0, 0.6), ylab = 'Probability', main = 'Histogram of Sepal.Length"')
lines(col= "red", density(anal_df$Sepal.Length, na.rm = T, from = min(anal_df$Sepal.Length), to = max(anal_df$Sepal.Length)))

hist(anal_df$Sepal.Width, breaks = 15, freq = F, xlab = 'Sepal.Width', ylim = c(0, 1.5), ylab = 'Probability', main = 'Histogram of Sepal.Width')
lines(col= "red",density(anal_df$Sepal.Width, na.rm = T, from = min(anal_df$Sepal.Width), to = max(anal_df$"Sepal.Width")))

hist(anal_df$Petal.Length, breaks = 15, freq = F, xlab = 'Petal.Length', ylim = c(0, 0.6), ylab = 'Probability', main = 'Histogram of Petal.Length')
lines(col= "red",density(anal_df$Petal.Length, na.rm = T, from = min(anal_df$Petal.Length)*2, to = max(anal_df$Petal.Length)))

hist(anal_df$Petal.Width, breaks = 15, freq = F, xlab = 'Petal.Width', ylim = c(0, 1.5), ylab = 'Probability', main = 'Histogram of Petal.Width')
lines(col= "red",density(anal_df$Petal.Width, na.rm = T, from = min(anal_df$Petal.Width), to = max(anal_df$Petal.Width)))




##############################################
## Correlation Matrix Plot1
##############################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

pairs(anal_df,lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist,
      pch = 21, bg = c("black", "white","blue")[unclass(anal_df_c$Species)]) 

par(mfrow = c(2, 2))
qqnorm(anal_df$Sepal.Length, main="Sepal.Length Noraml Q-Q Plot", pch = 21, bg = c("black", "white")[unclass(anal_df$ox)]);qqline(anal_df$Sepal.Length, col = 2)
qqnorm(anal_df$Sepal.Width, main="Sepal.Width Noraml Q-Q Plot", pch = 21, bg = c("black", "white")[unclass(anal_df$ox)]);qqline(anal_df$Sepal.Width, col = 2)
qqnorm(anal_df$Petal.Length, main="Petal.Length Noraml Q-Q Plot", pch = 21, bg = c("black", "white")[unclass(anal_df$ox)]);qqline(anal_df$Petal.Length, col = 2)
qqnorm(anal_df$Petal.Width, main="Petal.Width Noraml Q-Q Plot", pch = 21, bg = c("black", "white")[unclass(anal_df$ox)]);qqline(anal_df$Petal.Width, col = 2)

qqnorm(anal_df$x51, main="x51 Noraml Q-Q Plot", pch = 21, bg = c("black", "white")[unclass(anal_df$ox)]);qqline(anal_df$x51, col = 2)


#normality test
#install.packages("nortest")
library(nortest)
pearson.test(anal_df$Sepal.Length);pearson.test(anal_df$Sepal.Width); pearson.test(anal_df$Petal.Length);pearson.test(anal_df$Petal.Width);pearson.test(anal_df$x51)
ad.test(anal_df$Sepal.Length);ad.test(anal_df$Sepal.Width); ad.test(anal_df$Petal.Length);ad.test(anal_df$Petal.Width);ad.test(anal_df$x51)



# histogram with kernel density estimate 
par(mfrow = c(2, 2))
hist(anal_df$Sepal.Length, breaks = 15, freq = F, xlab = 'Sepal.Length', ylim = c(0, 0.15), ylab = 'Probability', main = 'Histogram of Sepal.Length')
lines(col="red", density(anal_df$Sepal.Length, na.rm = T, from = min(anal_df$Sepal.Length), to = max(anal_df$Sepal.Length)))
hist(anal_df$Sepal.Width, breaks = 15, freq = F, xlab = 'Sepal.Width', ylim = c(0, 0.05), ylab = 'Probability', main = 'Histogram of Sepal.Width')
lines(col="red",density(anal_df$Sepal.Width, na.rm = T, from = min(anal_df$Sepal.Width), to = max(anal_df$Sepal.Width)))
hist(anal_df$Petal.Length, breaks = 15, freq = F, xlab = 'Petal.Length', ylim = c(0, 0.015), ylab = 'Probability', main = 'Histogram of Petal.Length')
lines(col="red",density(anal_df$Petal.Length, na.rm = T, from = min(anal_df$Petal.Length)*2, to = max(anal_df$Petal.Length)))
hist(anal_df$Petal.Width, breaks = 15, freq = F, xlab = 'Petal.Width', ylim = c(0, 0.05), ylab = 'Probability', main = 'Histogram of Petal.Width')
lines(col="red",density(anal_df$Petal.Width, na.rm = T, from = min(anal_df$Petal.Width), to = max(anal_df$Petal.Width)))



#########################################################
## outlier-detection and remove!
#########################################################
##http://www.rdatamining.com/examples/outlier-detection
##LOF (Local Outlier Factor) is an algorithm for identifying 
##density-based local outliers [Breunig et al., 2000]. 
#########################################################
install.packages("DMwR")
library(DMwR)
anal_df <- read.csv("DAT\\160326_anal_df_V20.csv", header = TRUE, stringsAsFactors = FALSE)

outlier.scores <- lofactor(anal_df, k=5)
plot(density(outlier.scores))
plot(outlier.scores, type="b", ylim=c(0,3), main="Outliers")
abline(h = mean(outlier.scores)+ 3*sd(outlier.scores), col = "blue", lty=2)

# pick top 14 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:3]
# who are outliers
print(outliers)
anal_df <- anal_df[-c(42, 107,  23),]
nrow(anal_df)

################################################
# 1. K-means determining the number of clusters
################################################
install.packages("NbClust")
library(NbClust)
res <- NbClust(anal_df, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="all")

res$All.index
res$Best.nc
res$All.CriticalValues
res$Best.partition

jitter <- rnorm(nrow(anal_df), mean=0, sd=0.05)
res$Best.partition <- res$Best.partition+jitter 
plot(res$Best.partition, ylim=c(0,3.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) 
abline(v = 50, col = "blue", lty=2);abline(v = 100, col = "blue", lty=2);






################################################
# 1. K-means ED Clustering and Silhouette Plot
################################################

# 데이터셋 저장하기
#data_df <- read.table("clipboard", header = T, sep = "\t", stringsAsFactor = F)
#write.csv(data_df, "DAT\\160326_anal_df_V20.csv", row.names = F)

# 파생 데이터셋 불러오기
#anal_df <- read.csv("DAT\\160326_anal_df_V20.csv", header = TRUE, stringsAsFactors = FALSE)
str(anal_df)
head(anal_df)

###############################################
# K-means ED Silhouette Plot
dissE <- daisy(anal_df)  

par(mfrow = c(2, 2))
kme_df2 <- kmeans(anal_df, 2)   # K=2, random initial value
centers2 <- kme_df2$centers     # K=2, centers
kme_df2 <- kmeans(anal_df, centers2)# K=2, centers initial value
centers2 <- kme_df2$centers     # K=2, centers
kme_df2 <- kmeans(anal_df, centers2)# K=2, centers initial value
silhouette.kme2 <- silhouette(kme_df2$cl, dissE)
plot(silhouette.kme2, main="Silhouette K-means_ED  (K=2)", do.n.k=FALSE, col = c6[1:2]  )
kme_df3 <- kmeans(anal_df, 3)
centers3 <- kme_df3$centers 
kme_df3 <- kmeans(anal_df, centers3)
centers3 <- kme_df3$centers 
kme_df3 <- kmeans(anal_df, centers3)
silhouette.kme3 <- silhouette(kme_df3$cl, dissE)
plot(silhouette.kme3, main="Silhouette K-means_ED  (K=3)", do.n.k=FALSE, col = c6[1:3]  )
kme_df4 <- kmeans(anal_df, 4)
centers4 <- kme_df4$centers 
kme_df4 <- kmeans(anal_df, centers4)
centers4 <- kme_df4$centers 
kme_df4 <- kmeans(anal_df, centers4)
silhouette.kme4 <- silhouette(kme_df4$cl, dissE)
plot(silhouette.kme4, main="Silhouette K-means_ED  (K=4)", do.n.k=FALSE, col = c6[1:4]  )
kme_df5 <- kmeans(anal_df, 5)   
centers5 <- kme_df5$centers 
kme_df5 <- kmeans(anal_df, centers5)
centers5 <- kme_df5$centers 
kme_df5 <- kmeans(anal_df, centers5)
silhouette.kme5 <- silhouette(kme_df5$cl, dissE)
plot(silhouette.kme5, main="Silhouette K-means_ED  (K=5)", do.n.k=FALSE, col = c6[1:5]  )

###############################################
# K-means ED Cluster Plot
par(mfrow = c(2, 2))
jitter <- rnorm(nrow(anal_df), mean=0, sd=0.05)
kme_jit2 <- kme_df2$cluster+jitter 
plot(kme_jit2,  xlab="Time Sequence", ylab="Clusters by K-means(ED)",main="K-means_ED (K=2)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
kme_jit3 <- kme_df3$cluster+jitter 
plot(kme_jit3, xlab="Time Sequence", ylab="Clusters by K-means(ED)",main="K-means_ED (K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
kme_jit4 <- kme_df4$cluster+jitter 
plot(kme_jit4, xlab="Time Sequence", ylab="Clusters by K-means(ED)",main="K-means_ED (K=4)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
kme_jit5 <- kme_df5$cluster+jitter 
plot(kme_jit5, xlab="Time Sequence", ylab="Clusters by K-means(ED)",main="K-means_ED (K=5)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

###############################################
## PCA Plot - K-means(ED)
pca <- princomp(anal_df, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
pc.comp3 <- -1*pc.comp[,3] # principal component 1 scores (negated for convenience)
pc.comp4 <- -1*pc.comp[,4] # principal component 2 scores (negated for convenience)
X <- cbind(pc.comp1, pc.comp2)
plot(pca)
par(mfrow = c(2, 2))
plot(pc.comp1, pc.comp2, pch= kme_df2$cluster, col=kme_df2$cluster, main="K-means(K=2, ED)")
plot(pc.comp1, pc.comp2, pch= kme_df3$cluster, col=kme_df3$cluster, main="K-means(K=3, ED)")
plot(pc.comp1, pc.comp2, pch= kme_df4$cluster, col=kme_df4$cluster, main="K-means(K=4, ED)")
plot(pc.comp1, pc.comp2, pch= kme_df5$cluster, col=kme_df5$cluster, main="K-means(K=5, ED)")


kme <- cbind(pc.comp1, pc.comp2, kme_df2$cluster)
kme <- as.data.frame(kme)
c1_name = kme[,1]
c2_name = kme[,2]
matplot(c1_name, c2_name, type="n", xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name)))
i1 = kme$class =="1"
i2 = kme$class =="2"
matplot(kme[i1, 1],kme[i1, 2], pch="1", col="red", cex=0.5, xlab="pc.comp1",ylab="pc.comp2") 
matpoints(kme[i2, 1],kme[i2, 2], pch="2", col="blue", cex=0.5) 



par(mfrow = c(2, 2))
kme2 <- cbind(pc.comp1, pc.comp2, kme_df2$cluster)
colnames(kme2) <- c("pc.comp1","pc.comp2","class")
kme2 <- as.data.frame(kme2)
c1_name = kme2[,1]
c2_name = kme2[,2]
i1 = kme2$class =="1"
i2 = kme2$class =="2"
matplot(kme2[i1, 1],kme[i1, 2], pch="1", col="red", cex=0.5, xlab="pc.comp1",ylab="pc.comp2",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme2[i2, 1],kme[i2, 2], pch="2", col="blue", cex=0.5) 

kme3 <- cbind(pc.comp1, pc.comp2, kme_df3$cluster)
colnames(kme3) <- c("pc.comp1","pc.comp2","class")
kme3 <- as.data.frame(kme3)
c1_name = kme3[,1]
c2_name = kme3[,2]
i1 = kme3$class =="1"
i2 = kme3$class =="2"
i3 = kme3$class =="3"
matplot(kme3[i1, 1],kme[i1, 2], pch="1", col="red", cex=0.5, xlab="pc.comp1",ylab="pc.comp2",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme3[i2, 1],kme[i2, 2], pch="2", col="blue", cex=0.5) 
matpoints(kme3[i3, 1],kme[i3, 2], pch="3", col="green", cex=0.5) 

kme4 <- cbind(pc.comp1, pc.comp2, kme_df4$cluster)
colnames(kme4) <- c("pc.comp1","pc.comp2","class")
kme4 <- as.data.frame(kme4)
c1_name = kme4[,1]
c2_name = kme4[,2]
i1 = kme4$class =="1"
i2 = kme4$class =="2"
i3 = kme4$class =="3"
i4 = kme4$class =="4"
matplot(kme4[i1, 1],kme[i1, 2], pch="1", col="red", cex=0.5, xlab="pc.comp1",ylab="pc.comp2",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme4[i2, 1],kme[i2, 2], pch="2", col="blue", cex=0.5) 
matpoints(kme4[i3, 1],kme[i3, 2], pch="3", col="green", cex=0.5) 
matpoints(kme4[i4, 1],kme[i4, 2], pch="4", col="black", cex=0.5) 

kme5 <- cbind(pc.comp1, pc.comp2, kme_df5$cluster)
colnames(kme5) <- c("pc.comp1","pc.comp2","class")
kme5 <- as.data.frame(kme5)
c1_name = kme5[,1]
c2_name = kme5[,2]
i1 = kme5$class =="1"; i2 = kme5$class =="2"
i3 = kme5$class =="3"; i4 = kme5$class =="4"; i5 = kme5$class =="5"
matplot(kme5[i1, 1],kme[i1, 2], pch="1", col="red", cex=0.5, xlab="pc.comp1",ylab="pc.comp2",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme5[i2, 1],kme[i2, 2], pch="2", col="blue", cex=0.5) 
matpoints(kme5[i3, 1],kme[i3, 2], pch="3", col="green", cex=0.5) 
matpoints(kme5[i4, 1],kme[i4, 2], pch="4", col="black", cex=0.5) 
matpoints(kme5[i5, 1],kme[i5, 2], pch="5", col="pink", cex=0.5) 




############################################### 
##Centroid Plot against 1st 2 discriminant functions
library(fpc)
par(mfrow = c(2, 2))
plotcluster(anal_df[,-c(3)] kme_df2$cluster, cex=.6, main="K-means_ED (K=2)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df3$cluster, cex=.6, main="K-means_ED (K=3)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df4$cluster, cex=.6, main="K-means_ED (K=4)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df5$cluster, cex=.6, main="K-means_ED (K=5)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")




par(mfrow = c(2, 2))
kme_cluster2 <- cbind(anal_df$Sepal.Length, anal_df$Sepal.Width, kme_df2$cluster)
colnames(kme_cluster2) <- c("Sepal.Length","Sepal.Width","class")
kme_cluster2 <- as.data.frame(kme_cluster2)
c1_name = kme_cluster2[,1]
c2_name = kme_cluster2[,2]
i1 = kme_cluster2$class =="1"
i2 = kme_cluster2$class =="2"
matplot(kme_cluster2[i1, 1],kme_cluster2[i1, 2], pch="1", col="red", cex=0.5, xlab="Sepal.Length",ylab="x12",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme_cluster2[i2, 1],kme_cluster2[i2, 2], pch="2", col="blue", cex=0.5) 

kme_cluster3 <- cbind(anal_df$Petal.Length, anal_df$Petal.Width, kme_df3$cluster)
colnames(kme_cluster3) <- c("Petal.Length","Petal.Width","class")
kme_cluster3 <- as.data.frame(kme_cluster2)
c1_name = kme_cluster3[,1]
c2_name = kme_cluster3[,2]
i1 = kme_cluster3$class =="1"
i2 = kme_cluster3$class =="2"
i3 = kme_cluster3$class =="3"
matplot(kme_cluster3[i1, 1],kme_cluster3[i1, 2], pch="1", col="red", cex=0.5, xlab="Sepal.Length",ylab="x12",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme_cluster3[i2, 1],kme_cluster3[i2, 2], pch="2", col="blue", cex=0.5) 
matpoints(kme_cluster3[i3, 1],kme_cluster3[i3, 2], pch="3", col="blue", cex=0.5) 


kme_cluster2 <- cbind(anal_df$Sepal.Length, anal_df$Sepal.Width, kme_df2$cluster)
colnames(kme_cluster2) <- c("Sepal.Length","Sepal.Width","class")
kme_cluster2 <- as.data.frame(kme_cluster2)
c1_name = kme_cluster2[,1]
c2_name = kme_cluster2[,2]
i1 = kme_cluster2$class =="1"
i2 = kme_cluster2$class =="2"
matplot(kme_cluster2[i1, 1],kme_cluster2[i1, 2], pch="1", col="red", cex=0.5, xlab="Sepal.Length",ylab="x12",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme_cluster2[i2, 1],kme_cluster2[i2, 2], pch="2", col="blue", cex=0.5) 

kme_cluster2 <- cbind(anal_df$Sepal.Length, anal_df$Sepal.Width, kme_df2$cluster)
colnames(kme_cluster2) <- c("Sepal.Length","Sepal.Width","class")
kme_cluster2 <- as.data.frame(kme_cluster2)
c1_name = kme_cluster2[,1]
c2_name = kme_cluster2[,2]
i1 = kme_cluster2$class =="1"
i2 = kme_cluster2$class =="2"
matplot(kme_cluster2[i1, 1],kme_cluster2[i1, 2], pch="1", col="red", cex=0.5, xlab="Sepal.Length",ylab="x12",xlim=c(min(c1_name), max(c1_name)),ylim=c(min(c2_name), max(c2_name))) 
matpoints(kme_cluster2[i2, 1],kme_cluster2[i2, 2], pch="2", col="blue", cex=0.5) 















###############################################
## K=3 Silhouette plot and jitter plot
###############################################
par(mfrow = c(2, 2))
plot(silhouette.kme3, main="Silhouette K-means_ED  (K=3)", do.n.k=FALSE, col = c6[1:3]  )
plot(kme_jit3, xlab="Time Sequence", ylab="Clusters by K-means(ED)",main="K-means_ED (K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
plot(silhouette.mks3, main="Silhouette K-means_MD  (K=3)", do.n.k=FALSE, col = c6[1:3]  )
plot(mks_jit3, xlab="Time Sequence", ylab="Clusters by K-means(MD)",main="K-means_MD (K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

par(mfrow = c(2, 2))
plot(silhouette.pam3, main="Silhouette PAM_ED  (K=3)" , do.n.k=FALSE, col = c6[1:3])
plot(pam_jit3, xlab="Time Sequence", ylab="Clusters by PAM(ED)",main="PAM_ED(K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
plot(silhouette.pam_MD3, main="Silhouette PAM_MD (K=3)", do.n.k=FALSE, col = c6[1:3]  )
plot(pam_md_jit3, xlab="Time Sequence", ylab="Clusters by PAM(MD)",main="PAM_MD(K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

#################################################################
## K=3 Centroid Plot against 1st 2 discriminant functions
#################################################################
# Centroid Plot against 1st 2 discriminant functions
par(mfrow = c(2, 2))
plotcluster(anal_df, kme_df3$cluster, cex=.6, main="K-means_ED (K=3)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, mks_df3$Cluster, cex=.6, main="K-means_MD (K=3)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result3$clustering, cex=.6, main="PAM_ED (K=3)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df3$Cluster, cex=.6, main="PAM_MD (K=3)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")








par(mfrow = c(1, 2))
plotcluster(anal_df, res$Best.partition, cex=.6, main="K-means_MD (K=3)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df3$Cluster, cex=.6, main="PAM_MD (K=3)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")

par(mfrow = c(4, 4))
plotcluster(anal_df, kme_df2$cluster, cex=.6, main="K-means_ED (K=2)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df3$cluster, cex=.6, main="K-means_ED (K=3)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df4$cluster, cex=.6, main="K-means_ED (K=4)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df5$cluster, cex=.6, main="K-means_ED (K=5)", xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")#, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, mks_df2$Cluster, cex=.6, main="K-means_MD (K=2)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate") 
plotcluster(anal_df, mks_df3$Cluster, cex=.6, main="K-means_MD (K=3)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df4$Cluster, cex=.6, main="K-means_MD (K=4)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df5$Cluster, cex=.6, main="K-means_MD (K=5)",xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result2$clustering, cex=.6, main="PAM_ED (K=2)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result3$clustering, cex=.6, main="PAM_ED (K=3)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result4$clustering, cex=.6, main="PAM_ED (K=4)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result5$clustering, cex=.6, main="PAM_ED (K=5)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df2$Cluster, cex=.6, main="PAM_MD (K=2)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df3$Cluster, cex=.6, main="PAM_MD (K=3)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df4$Cluster, cex=.6, main="PAM_MD (K=4)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, clust_df5$Cluster, cex=.6, main="PAM_MD (K=5)",xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")

























################################################
# 2. K-means MD Clustering and Silhouette Plot
################################################
library(MASS)
init.x    <- 2 ## 초기 중심값 설정 방법론 옵션
group.v <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
#group.v <- c(1,1,2,2,3,3,4,4,5,5)
fs.m      <- "MD"## 변수 중요도 평가 방법론 옵션

# K-means MD Silhouette Plot
cluster.k <- 2
mks_df2 <- mks_fn(anal_df, cluster.k, init.x, group.v, fs.m)
dissE <- daisy(anal_df)
silhouette.mks2 <- silhouette(mks_df2$Cluster, dissE)
cluster.k <- 3
mks_df3 <- mks_fn(anal_df, cluster.k, init.x, group.v, fs.m)
dissE <- daisy(anal_df)
silhouette.mks3 <- silhouette(mks_df3$Cluster, dissE)
cluster.k <- 4
mks_df4 <- mks_fn(anal_df, cluster.k, init.x, group.v, fs.m)
dissE <- daisy(anal_df)
silhouette.mks4 <- silhouette(mks_df4$Cluster, dissE)
cluster.k <- 5
mks_df5 <- mks_fn(anal_df, cluster.k, init.x, group.v, fs.m)
dissE <- daisy(anal_df)
silhouette.mks5 <- silhouette(mks_df5$Cluster, dissE)

par(mfrow = c(2, 2))
plot(silhouette.mks2, main="Silhouette K-means_MD  (K=2)", do.n.k=FALSE, col = c6[1:2]  )
plot(silhouette.mks3, main="Silhouette K-means_MD  (K=3)", do.n.k=FALSE, col = c6[1:3]  )
plot(silhouette.mks4, main="Silhouette K-means_MD  (K=4)", do.n.k=FALSE, col = c6[1:4]  )
plot(silhouette.mks5, main="Silhouette K-means_MD  (K=5)", do.n.k=FALSE, col = c6[1:5]  )

# K-means MD Cluster Plot
par(mfrow = c(2, 2))
jitter <- rnorm(nrow(anal_df), mean=0, sd=0.05)
mks_jit2 <- mks_df2$Cluster+jitter 
plot(mks_jit2, xlab="Time Sequence", ylab="Clusters by K-means(MD)",main="K-means_MD (K=2)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
mks_jit3 <- mks_df3$Cluster+jitter 
plot(mks_jit3, xlab="Time Sequence", ylab="Clusters by K-means(MD)",main="K-means_MD (K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
mks_jit4 <- mks_df4$Cluster+jitter 
plot(mks_jit4, xlab="Time Sequence", ylab="Clusters by K-means(MD)",main="K-means_MD (K=4)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
mks_jit5 <- mks_df5$Cluster+jitter 
plot(mks_jit5, xlab="Time Sequence", ylab="Clusters by K-means(MD)",main="K-means_MD (K=5)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 


#mks2.result <-  mks_fn(anal_df, 2)
#layout(matrix(c(1, 2), 1, 2)) # 2 graphs per page
#plot(mks2.result)


# Centroid Plot against 1st 2 discriminant functions
library(fpc)
par(mfrow = c(2, 2))
plotcluster(anal_df, mks_df2$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate") 
plotcluster(anal_df, mks_df3$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df4$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df5$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")






















## PCA Plot - K-means(MD)
pca <- princomp(anal_df, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
pc.comp3 <- -1*pc.comp[,3] # principal component 1 scores (negated for convenience)
pc.comp4 <- -1*pc.comp[,4] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
#plot(pca)
par(mfrow = c(2, 2))
plot(pc.comp1, pc.comp2, pch= mks_df2$Cluster, col=mks_df2$Cluster, main="K-means(K=2, MD)")
#points(mks_df2$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= mks_df3$Cluster, col=mks_df3$Cluster, main="K-means(K=3, MD)")
#points(mks_df3$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= mks_df4$Cluster, col=mks_df4$Cluster, main="K-means(K=4, MD)")
#points(mks_df4$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= mks_df5$Cluster, col=mks_df5$Cluster, main="K-means(K=5, MD)")
#points(mks_df5$centers, pch=20)




################################################
# 3. PAM ED Clustering and Silhouette Plot
################################################
library(cluster)

# PAM ED Silhouette Plot
cluster.k <- 2
pam.result2 <- pam(anal_df, cluster.k)
dissE <- daisy(anal_df)
silhouette.pam2 <- silhouette(pam.result2$clustering, dissE)
cluster.k <- 3
pam.result3 <- pam(anal_df, cluster.k)
dissE <- daisy(anal_df)
silhouette.pam3 <- silhouette(pam.result3$clustering, dissE)
cluster.k <- 4
pam.result4 <- pam(anal_df, cluster.k)
dissE <- daisy(anal_df)
silhouette.pam4 <- silhouette(pam.result4$clustering, dissE)
cluster.k <- 5
pam.result5 <- pam(anal_df, cluster.k)
dissE <- daisy(anal_df)
silhouette.pam5 <- silhouette(pam.result5$clustering, dissE)


par(mfrow = c(2, 2))
plot(silhouette.pam2, main="Silhouette PAM_ED  (K=2)" )
plot(silhouette.pam3, main="Silhouette PAM_ED  (K=3)" )
plot(silhouette.pam4, main="Silhouette PAM_ED  (K=4)" )
plot(silhouette.pam5, main="Silhouette PAM_ED  (K=5)" )


c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20")
par(mfrow = c(2, 2))
for(k in 2:5)
  plot(silhouette(pam(anal_df, k=k)), main = paste("Silhouette PAM_ED, K =",k), do.n.k=FALSE, col = c6[1:k])





# PAM ED Cluster Plot
par(mfrow = c(2, 2))
jitter <- rnorm(nrow(anal_df), mean=0, sd=0.05)
pam_jit2 <- pam.result2$clustering+jitter 
plot(pam_jit2, xlab="Time Sequence", ylab="Clusters by PAM(ED)",main="PAM_ED(K=2)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
pam_jit3 <- pam.result3$clustering+jitter 
plot(pam_jit3, xlab="Time Sequence", ylab="Clusters by PAM(ED)",main="PAM_ED(K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
pam_jit4 <- pam.result4$clustering+jitter 
plot(pam_jit4, xlab="Time Sequence", ylab="Clusters by PAM(ED)",main="PAM_ED(K=4)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 
pam_jit5 <- pam.result5$clustering+jitter 
plot(pam_jit5, xlab="Time Sequence", ylab="Clusters by PAM(ED)",main="PAM_ED(K=5)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

### Centroid Plot against 1st 2 discriminant functions
library(fpc)
par(mfrow = c(2, 2))
plotcluster(anal_df, pam.result2$clustering, cex=.6, xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result3$clustering, cex=.6, xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result4$clustering, cex=.6, xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, pam.result5$clustering, cex=.6, xlab="1st Discriminant Coordinate", ylab="2nd Discriminant Coordinate")

par(mfrow = c(2, 2))
plotcluster(anal_df, mks_df2$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate") 
plotcluster(anal_df, mks_df3$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df4$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")
plotcluster(anal_df, mks_df5$Cluster, cex=.6, xlab="1st Discriminant Coordinate",ylab="2nd Discriminant Coordinate")



pam.result2$clustering <- as.numeric(pam.result2$clustering)




## PCA Plot - PAM_ED
pca <- princomp(anal_df, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
pc.comp3 <- -1*pc.comp[,3] # principal component 1 scores (negated for convenience)
pc.comp4 <- -1*pc.comp[,4] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
plot(pca)
par(mfrow = c(2, 2))
plot(pc.comp1, pc.comp2, pch= pam.result2$clustering, col=pam.result2$clustering, main="PAM(K=2, ED)")
#points(pam.result2$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= pam.result3$clustering, col=pam.result3$clustering, main="PAM(K=3, ED)")
#points(pam.result3$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= pam.result4$clustering, col=pam.result4$clustering, main="PAM(K=4, ED)")
#points(pam.result4$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= pam.result5$clustering, col=pam.result5$clustering, main="PAM(K=5, ED)")
#points(pam.result5$centers, pch=20)






################################################
# 4. PAM MD Clustering and Silhouette Plot
################################################

library(MASS)

# 파생 데이터셋 불러오기
data_df <- read.csv("DAT\\160326_anal_df_V20.csv", header = TRUE, stringsAsFactors = FALSE)
head(data_df)

#data_df <- data_df[ , c(1:5)]

data_df <- anal_df

# 객체 개수 계산
count.x <- nrow(data_df)
# 군집개수 정의
cluster.k <- 2

#=======================================================================
# 분석(Build)
#=======================================================================
# 객체들 간의 거리 계산
dist_df <- matrix(data = NA, nrow = count.x, ncol = count.x)

for(i in 1:nrow(data_df)) {
  dist_df[ , i] <- mahal_fn(x = data_df, 
                            center = as.numeric(data_df[i, ]),
                            cor = cor(data_df))
}
dist_df <- as.data.frame(dist_df)

# 거리합 산출
dist_df$SUM_DIST <- rowSums(dist_df)

# 거리합이 가장 작은 객체 선정
main.x <- which.min(dist_df$SUM_DIST)

#----------------------------------
# 대표객체 추가 선정
# 데이터 변환
dist_mat <- as.matrix(dist_df[, -length(dist_df)])
dimnames(dist_mat)[[1]] <- 1:count.x
dimnames(dist_mat)[[2]] <- 1:count.x

tmp_df <- data.frame(후보 = as.numeric(colnames(dist_mat)[col(dist_mat)]), 
                       객체 = as.numeric(rownames(dist_mat)[row(dist_mat)]),
                       거리 = c(dist_mat))
#----------------------------------
# for loop (군집개수 만큼)

for(i in 1:(cluster.k - 1)) {
  # 후보객체 선정
  main.y <- c(1:count.x)[-main.x]
  # 선택된 대표객체와의 거리 계산치 저장
  xx <- dist_df[main.x, -c(main.x, ncol(dist_df))]
  yy <- apply(xx, 2, min)
  dist.x <- as.numeric(yy)
  # 후보객체와의 거리 계산치 저장
  dist.y <- subset(tmp_df, !(후보 %in% main.x | 객체 %in% main.x))
  #----------------------------------
  # 거리 감소량 계산
  tmp.x <- ifelse((dist.x - dist.y$거리) > 0, (dist.x - dist.y$거리), 0)
  build_df <- data.frame(dist.y, 기존거리 = dist.x, 거리감소량 = tmp.x)
  #----------------------------------
  # 대표객체 추가 선정
  add.x <- which.max(aggregate(거리감소량 ~ 후보, build_df, sum)$거리감소량)
  main.x <- c(main.x, add.x)
}

#=======================================================================
# 분석(Swap)
#----------------------------------
# 목적함수 변화량 계산 Loop
## 반복횟수 저장
iter.x <- 0

## Loop Stop 조건 초기값 설정
loop.x <- -1

while(loop.x < 0) {
  
  # 객체 정의
  main.x# 대표 객체
  main.y <- c(1:count.x)[-main.x]# 교환 객체
  
  #----------------------------------
  ## swap 데이터 셋 셋팅_1
  ## 교환, 후보, 객체간 거리 정의
  
  tmp2_df <- subset(tmp_df, 후보 %in% main.x & 후보 != 객체)
  tmp3_df <- subset(tmp_df, 후보 %in% main.y & 후보 != 객체 & !(객체 %in% main.x))
  
  swap1_df <- merge(tmp2_df, tmp3_df[ , -3], by = "객체")
  
  swap2_df <- merge(swap1_df, tmp_df, by.x = c("객체", "후보.y"),
                    by.y = c("객체", "후보"))
  names(swap2_df) <- c("객체_j", "교환_h", "대표_i", "거리_ij", "거리_hj")
  
  #----------------------------------
  ## Dj, Ej 계산
  dj.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[1])
  ej.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[2]) 
  swap3_df <- Reduce(function(x, y) merge(x, y, by.x = "객체_j", by.y = "객체"),
                     list(swap2_df, dj.x, ej.x))
  swap3_df <- swap3_df[order(swap3_df$대표_i, swap3_df$교환_h), ]
  names(swap3_df)[6:7] <- c("Dj", "Ej")
  
  #----------------------------------
  ## 조건 탐색
  ## 목적함수 저장 오브젝트 생성
  swap3_df$Cjih <- NA
  attach(swap3_df)
  con.a <- 거리_ij > Dj & 거리_hj >= Dj
  swap3_df$Cjih[con.a] <- 0
  
  con.b1 <- 거리_ij <= Dj & 거리_hj < Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b1] <- 거리_hj[con.b1] - 거리_ij[con.b1]
  
  con.b2 <- 거리_ij <= Dj & 거리_hj >= Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b2] <- Ej[con.b2] - 거리_ij[con.b2]
  
  con.c <- 거리_ij >= Dj & 거리_hj < Dj & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.c] <- 거리_hj[con.c] - Dj[con.c]
  detach(swap3_df)
  
  ## 총 변화량 계산
  Tih.tmp <- aggregate(Cjih ~ 교환_h + 대표_i, swap3_df, sum)
  Tih.min <- Tih.tmp[which.min(Tih.tmp[ , 3]), ]
  if(Tih.min[3] < 0) {
    main.x[which(main.x %in% Tih.min[, 2])] <- Tih.min[, 1]
  } else {
    main.x <- main.x 
  }
  iter.x <- iter.x + 1
  loop.x <- Tih.min[3]
  print(loop.x)
}

## 군집 할당
clust.tmp <- subset(tmp_df, 후보 %in% main.x)# 후보 = 대표 객체
clust_df2 <- aggregate(거리 ~ 객체, clust.tmp, which.min)
names(clust_df2) <- c("객체", "Cluster")


cluster.k <- 3
#=======================================================================
# 분석(Build)
#=======================================================================
# 객체들 간의 거리 계산
dist_df <- matrix(data = NA, nrow = count.x, ncol = count.x)

for(i in 1:nrow(data_df)) {
  dist_df[ , i] <- mahal_fn(x = data_df, 
                            center = as.numeric(data_df[i, ]),
                            cor = cor(data_df))
}
dist_df <- as.data.frame(dist_df)

# 거리합 산출
dist_df$SUM_DIST <- rowSums(dist_df)

# 거리합이 가장 작은 객체 선정
main.x <- which.min(dist_df$SUM_DIST)

#----------------------------------
# 대표객체 추가 선정
# 데이터 변환
dist_mat <- as.matrix(dist_df[, -length(dist_df)])
dimnames(dist_mat)[[1]] <- 1:count.x
dimnames(dist_mat)[[2]] <- 1:count.x

tmp_df <- data.frame(후보 = as.numeric(colnames(dist_mat)[col(dist_mat)]), 
                       객체 = as.numeric(rownames(dist_mat)[row(dist_mat)]),
                       거리 = c(dist_mat))
#----------------------------------
# for loop (군집개수 만큼)

for(i in 1:(cluster.k - 1)) {
  # 후보객체 선정
  main.y <- c(1:count.x)[-main.x]
  # 선택된 대표객체와의 거리 계산치 저장
  xx <- dist_df[main.x, -c(main.x, ncol(dist_df))]
  yy <- apply(xx, 2, min)
  dist.x <- as.numeric(yy)
  # 후보객체와의 거리 계산치 저장
  dist.y <- subset(tmp_df, !(후보 %in% main.x | 객체 %in% main.x))
  #----------------------------------
  # 거리 감소량 계산
  tmp.x <- ifelse((dist.x - dist.y$거리) > 0, (dist.x - dist.y$거리), 0)
  build_df <- data.frame(dist.y, 기존거리 = dist.x, 거리감소량 = tmp.x)
  #----------------------------------
  # 대표객체 추가 선정
  add.x <- which.max(aggregate(거리감소량 ~ 후보, build_df, sum)$거리감소량)
  main.x <- c(main.x, add.x)
}

#=======================================================================
# 분석(Swap)
#----------------------------------
# 목적함수 변화량 계산 Loop
## 반복횟수 저장
iter.x <- 0

## Loop Stop 조건 초기값 설정
loop.x <- -1

while(loop.x < 0) {
  
  # 객체 정의
  main.x# 대표 객체
  main.y <- c(1:count.x)[-main.x]# 교환 객체
  
  #----------------------------------
  ## swap 데이터 셋 셋팅_1
  ## 교환, 후보, 객체간 거리 정의
  
  tmp2_df <- subset(tmp_df, 후보 %in% main.x & 후보 != 객체)
  tmp3_df <- subset(tmp_df, 후보 %in% main.y & 후보 != 객체 & !(객체 %in% main.x))
  
  swap1_df <- merge(tmp2_df, tmp3_df[ , -3], by = "객체")
  
  swap2_df <- merge(swap1_df, tmp_df, by.x = c("객체", "후보.y"),
                    by.y = c("객체", "후보"))
  names(swap2_df) <- c("객체_j", "교환_h", "대표_i", "거리_ij", "거리_hj")
  
  #----------------------------------
  ## Dj, Ej 계산
  dj.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[1])
  ej.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[2]) 
  swap3_df <- Reduce(function(x, y) merge(x, y, by.x = "객체_j", by.y = "객체"),
                     list(swap2_df, dj.x, ej.x))
  swap3_df <- swap3_df[order(swap3_df$대표_i, swap3_df$교환_h), ]
  names(swap3_df)[6:7] <- c("Dj", "Ej")
  
  #----------------------------------
  ## 조건 탐색
  ## 목적함수 저장 오브젝트 생성
  swap3_df$Cjih <- NA
  attach(swap3_df)
  con.a <- 거리_ij > Dj & 거리_hj >= Dj
  swap3_df$Cjih[con.a] <- 0
  
  con.b1 <- 거리_ij <= Dj & 거리_hj < Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b1] <- 거리_hj[con.b1] - 거리_ij[con.b1]
  
  con.b2 <- 거리_ij <= Dj & 거리_hj >= Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b2] <- Ej[con.b2] - 거리_ij[con.b2]
  
  con.c <- 거리_ij >= Dj & 거리_hj < Dj & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.c] <- 거리_hj[con.c] - Dj[con.c]
  detach(swap3_df)
  
  ## 총 변화량 계산
  Tih.tmp <- aggregate(Cjih ~ 교환_h + 대표_i, swap3_df, sum)
  Tih.min <- Tih.tmp[which.min(Tih.tmp[ , 3]), ]
  if(Tih.min[3] < 0) {
    main.x[which(main.x %in% Tih.min[, 2])] <- Tih.min[, 1]
  } else {
    main.x <- main.x 
  }
  iter.x <- iter.x + 1
  loop.x <- Tih.min[3]
  print(loop.x)
}
clust.tmp <- subset(tmp_df, 후보 %in% main.x)# 후보 = 대표 객체
clust_df3 <- aggregate(거리 ~ 객체, clust.tmp, which.min)
names(clust_df3) <- c("객체", "Cluster")

cluster.k <- 4
#=======================================================================
# 분석(Build)
#=======================================================================
# 객체들 간의 거리 계산
dist_df <- matrix(data = NA, nrow = count.x, ncol = count.x)

for(i in 1:nrow(data_df)) {
  dist_df[ , i] <- mahal_fn(x = data_df, 
                            center = as.numeric(data_df[i, ]),
                            cor = cor(data_df))
}
dist_df <- as.data.frame(dist_df)

# 거리합 산출
dist_df$SUM_DIST <- rowSums(dist_df)

# 거리합이 가장 작은 객체 선정
main.x <- which.min(dist_df$SUM_DIST)

#----------------------------------
# 대표객체 추가 선정
# 데이터 변환
dist_mat <- as.matrix(dist_df[, -length(dist_df)])
dimnames(dist_mat)[[1]] <- 1:count.x
dimnames(dist_mat)[[2]] <- 1:count.x

tmp_df <- data.frame(후보 = as.numeric(colnames(dist_mat)[col(dist_mat)]), 
                       객체 = as.numeric(rownames(dist_mat)[row(dist_mat)]),
                       거리 = c(dist_mat))
#----------------------------------
# for loop (군집개수 만큼)

for(i in 1:(cluster.k - 1)) {
  # 후보객체 선정
  main.y <- c(1:count.x)[-main.x]
  # 선택된 대표객체와의 거리 계산치 저장
  xx <- dist_df[main.x, -c(main.x, ncol(dist_df))]
  yy <- apply(xx, 2, min)
  dist.x <- as.numeric(yy)
  # 후보객체와의 거리 계산치 저장
  dist.y <- subset(tmp_df, !(후보 %in% main.x | 객체 %in% main.x))
  #----------------------------------
  # 거리 감소량 계산
  tmp.x <- ifelse((dist.x - dist.y$거리) > 0, (dist.x - dist.y$거리), 0)
  build_df <- data.frame(dist.y, 기존거리 = dist.x, 거리감소량 = tmp.x)
  #----------------------------------
  # 대표객체 추가 선정
  add.x <- which.max(aggregate(거리감소량 ~ 후보, build_df, sum)$거리감소량)
  main.x <- c(main.x, add.x)
}

#=======================================================================
# 분석(Swap)
#----------------------------------
# 목적함수 변화량 계산 Loop
## 반복횟수 저장
iter.x <- 0

## Loop Stop 조건 초기값 설정
loop.x <- -1

while(loop.x < 0) {
  
  # 객체 정의
  main.x# 대표 객체
  main.y <- c(1:count.x)[-main.x]# 교환 객체
  
  #----------------------------------
  ## swap 데이터 셋 셋팅_1
  ## 교환, 후보, 객체간 거리 정의
  
  tmp2_df <- subset(tmp_df, 후보 %in% main.x & 후보 != 객체)
  tmp3_df <- subset(tmp_df, 후보 %in% main.y & 후보 != 객체 & !(객체 %in% main.x))
  
  swap1_df <- merge(tmp2_df, tmp3_df[ , -3], by = "객체")
  
  swap2_df <- merge(swap1_df, tmp_df, by.x = c("객체", "후보.y"),
                    by.y = c("객체", "후보"))
  names(swap2_df) <- c("객체_j", "교환_h", "대표_i", "거리_ij", "거리_hj")
  
  #----------------------------------
  ## Dj, Ej 계산
  dj.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[1])
  ej.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[2]) 
  swap3_df <- Reduce(function(x, y) merge(x, y, by.x = "객체_j", by.y = "객체"),
                     list(swap2_df, dj.x, ej.x))
  swap3_df <- swap3_df[order(swap3_df$대표_i, swap3_df$교환_h), ]
  names(swap3_df)[6:7] <- c("Dj", "Ej")
  
  #----------------------------------
  ## 조건 탐색
  ## 목적함수 저장 오브젝트 생성
  swap3_df$Cjih <- NA
  attach(swap3_df)
  con.a <- 거리_ij > Dj & 거리_hj >= Dj
  swap3_df$Cjih[con.a] <- 0
  
  con.b1 <- 거리_ij <= Dj & 거리_hj < Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b1] <- 거리_hj[con.b1] - 거리_ij[con.b1]
  
  con.b2 <- 거리_ij <= Dj & 거리_hj >= Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b2] <- Ej[con.b2] - 거리_ij[con.b2]
  
  con.c <- 거리_ij >= Dj & 거리_hj < Dj & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.c] <- 거리_hj[con.c] - Dj[con.c]
  detach(swap3_df)
  
  ## 총 변화량 계산
  Tih.tmp <- aggregate(Cjih ~ 교환_h + 대표_i, swap3_df, sum)
  Tih.min <- Tih.tmp[which.min(Tih.tmp[ , 3]), ]
  if(Tih.min[3] < 0) {
    main.x[which(main.x %in% Tih.min[, 2])] <- Tih.min[, 1]
  } else {
    main.x <- main.x 
  }
  iter.x <- iter.x + 1
  loop.x <- Tih.min[3]
  print(loop.x)
}
clust.tmp <- subset(tmp_df, 후보 %in% main.x)# 후보 = 대표 객체
clust_df4 <- aggregate(거리 ~ 객체, clust.tmp, which.min)
names(clust_df4) <- c("객체", "Cluster")


cluster.k <- 5
#=======================================================================
# 분석(Build)
#=======================================================================
# 객체들 간의 거리 계산
dist_df <- matrix(data = NA, nrow = count.x, ncol = count.x)

for(i in 1:nrow(data_df)) {
  dist_df[ , i] <- mahal_fn(x = data_df, 
                            center = as.numeric(data_df[i, ]),
                            cor = cor(data_df))
}
dist_df <- as.data.frame(dist_df)

# 거리합 산출
dist_df$SUM_DIST <- rowSums(dist_df)

# 거리합이 가장 작은 객체 선정
main.x <- which.min(dist_df$SUM_DIST)

#----------------------------------
# 대표객체 추가 선정
# 데이터 변환
dist_mat <- as.matrix(dist_df[, -length(dist_df)])
dimnames(dist_mat)[[1]] <- 1:count.x
dimnames(dist_mat)[[2]] <- 1:count.x

tmp_df <- data.frame(후보 = as.numeric(colnames(dist_mat)[col(dist_mat)]), 
                       객체 = as.numeric(rownames(dist_mat)[row(dist_mat)]),
                       거리 = c(dist_mat))
#----------------------------------
# for loop (군집개수 만큼)

for(i in 1:(cluster.k - 1)) {
  # 후보객체 선정
  main.y <- c(1:count.x)[-main.x]
  # 선택된 대표객체와의 거리 계산치 저장
  xx <- dist_df[main.x, -c(main.x, ncol(dist_df))]
  yy <- apply(xx, 2, min)
  dist.x <- as.numeric(yy)
  # 후보객체와의 거리 계산치 저장
  dist.y <- subset(tmp_df, !(후보 %in% main.x | 객체 %in% main.x))
  #----------------------------------
  # 거리 감소량 계산
  tmp.x <- ifelse((dist.x - dist.y$거리) > 0, (dist.x - dist.y$거리), 0)
  build_df <- data.frame(dist.y, 기존거리 = dist.x, 거리감소량 = tmp.x)
  #----------------------------------
  # 대표객체 추가 선정
  add.x <- which.max(aggregate(거리감소량 ~ 후보, build_df, sum)$거리감소량)
  main.x <- c(main.x, add.x)
}

#=======================================================================
# 분석(Swap)
#----------------------------------
# 목적함수 변화량 계산 Loop
## 반복횟수 저장
iter.x <- 0

## Loop Stop 조건 초기값 설정
loop.x <- -1

while(loop.x < 0) {
  
  # 객체 정의
  main.x# 대표 객체
  main.y <- c(1:count.x)[-main.x]# 교환 객체
  
  #----------------------------------
  ## swap 데이터 셋 셋팅_1
  ## 교환, 후보, 객체간 거리 정의
  
  tmp2_df <- subset(tmp_df, 후보 %in% main.x & 후보 != 객체)
  tmp3_df <- subset(tmp_df, 후보 %in% main.y & 후보 != 객체 & !(객체 %in% main.x))
  
  swap1_df <- merge(tmp2_df, tmp3_df[ , -3], by = "객체")
  
  swap2_df <- merge(swap1_df, tmp_df, by.x = c("객체", "후보.y"),
                    by.y = c("객체", "후보"))
  names(swap2_df) <- c("객체_j", "교환_h", "대표_i", "거리_ij", "거리_hj")
  
  #----------------------------------
  ## Dj, Ej 계산
  dj.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[1])
  ej.x <- aggregate(거리 ~ 객체, tmp2_df, function(x) sort(x, FALSE)[2]) 
  swap3_df <- Reduce(function(x, y) merge(x, y, by.x = "객체_j", by.y = "객체"),
                     list(swap2_df, dj.x, ej.x))
  swap3_df <- swap3_df[order(swap3_df$대표_i, swap3_df$교환_h), ]
  names(swap3_df)[6:7] <- c("Dj", "Ej")
  
  #----------------------------------
  ## 조건 탐색
  ## 목적함수 저장 오브젝트 생성
  swap3_df$Cjih <- NA
  attach(swap3_df)
  con.a <- 거리_ij > Dj & 거리_hj >= Dj
  swap3_df$Cjih[con.a] <- 0
  
  con.b1 <- 거리_ij <= Dj & 거리_hj < Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b1] <- 거리_hj[con.b1] - 거리_ij[con.b1]
  
  con.b2 <- 거리_ij <= Dj & 거리_hj >= Ej & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.b2] <- Ej[con.b2] - 거리_ij[con.b2]
  
  con.c <- 거리_ij >= Dj & 거리_hj < Dj & is.na(swap3_df$Cjih)
  swap3_df$Cjih[con.c] <- 거리_hj[con.c] - Dj[con.c]
  detach(swap3_df)
  
  ## 총 변화량 계산
  Tih.tmp <- aggregate(Cjih ~ 교환_h + 대표_i, swap3_df, sum)
  Tih.min <- Tih.tmp[which.min(Tih.tmp[ , 3]), ]
  if(Tih.min[3] < 0) {
    main.x[which(main.x %in% Tih.min[, 2])] <- Tih.min[, 1]
  } else {
    main.x <- main.x 
  }
  iter.x <- iter.x + 1
  loop.x <- Tih.min[3]
  print(loop.x)
}
clust.tmp <- subset(tmp_df, 후보 %in% main.x)# 후보 = 대표 객체
clust_df5 <- aggregate(거리 ~ 객체, clust.tmp, which.min)
names(clust_df5) <- c("객체", "Cluster")



# PAM MD Silhouette Plot
dissE <- daisy(data_df)

silhouette.pam_MD2 <- silhouette(clust_df2$Cluster, dissE)
silhouette.pam_MD3 <- silhouette(clust_df3$Cluster, dissE)
silhouette.pam_MD4 <- silhouette(clust_df4$Cluster, dissE)
silhouette.pam_MD5 <- silhouette(clust_df5$Cluster, dissE) 

par(mfrow = c(2, 2))
plot(silhouette.pam_MD2, main="Silhouette PAM_MD (K=2)", do.n.k=FALSE, col = c6[1:2]  )
plot(silhouette.pam_MD3, main="Silhouette PAM_MD (K=3)", do.n.k=FALSE, col = c6[1:3]  )
plot(silhouette.pam_MD4, main="Silhouette PAM_MD (K=4)", do.n.k=FALSE, col = c6[1:4]  )
plot(silhouette.pam_MD5, main="Silhouette PAM_MD (K=5)", do.n.k=FALSE, col = c6[1:5]  )


# PAM MD Cluster Plot
clust_df2$Cluster
clust_df3$Cluster
clust_df4$Cluster
clust_df5$Cluster

par(mfrow = c(2, 2))
jitter <- rnorm(nrow(anal_df), mean=0, sd=0.05)

pam_md_jit2 <- clust_df2$Cluster+jitter 
plot(pam_md_jit2, xlab="Time Sequence", ylab="Clusters by PAM(MD)",main="PAM_MD(K=2)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

pam_md_jit3 <- clust_df3$Cluster+jitter 
plot(pam_md_jit3, xlab="Time Sequence", ylab="Clusters by PAM(MD)",main="PAM_MD(K=3)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

pam_md_jit4 <- clust_df4$Cluster+jitter 
plot(pam_md_jit4, xlab="Time Sequence", ylab="Clusters by PAM(MD)",main="PAM_MD(K=4)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 

pam_md_jit5 <- clust_df5$Cluster+jitter 
plot(pam_md_jit5, xlab="Time Sequence", ylab="Clusters by PAM(MD)",main="PAM_MD(K=5)", ylim=c(0.5,5.5))
abline(h = 1.5, col = "blue", lty=2); abline(h = 2.5, col = "blue", lty=2) ;abline(h = 3.5, col = "blue", lty=2); abline(h = 4.5, col = "blue", lty=2) ;abline(v = 200, col = "red", lty=3) 


##
# Centroid Plot against 1st 2 discriminant functions
library(fpc)
par(mfrow = c(2, 2))
plotcluster(anal_df, clust_df2$Cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, clust_df3$Cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, clust_df4$Cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, clust_df5$Cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")





## PCA Plot - PAM(MD)
pca <- princomp(anal_df, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
pc.comp3 <- -1*pc.comp[,3] # principal component 1 scores (negated for convenience)
pc.comp4 <- -1*pc.comp[,4] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
#plot(pca)
par(mfrow = c(2, 2))
plot(pc.comp1, pc.comp2, pch= clust_df2$Cluster, col=clust_df2$Cluster, main="PAM(K=2, MD)")
points(clust_df2$centers, pch=20)
legend("topright", "cluster 1, △ cluster 2", pch = 2)

plot(pc.comp1, pc.comp2, pch= clust_df3$Cluster, col=clust_df3$Cluster, main="PAM(K=3, MD)")
points(clust_df3$centers, pch=20)
legend("topright", "cluster 1, △ cluster 2, + cluster 3", pch = 2)

plot(pc.comp1, pc.comp2, pch= clust_df4$Cluster, col=clust_df4$Cluster, main="PAM(K=4, MD)")
points(clust_df4$centers, pch=20)
legend("topright", "cluster 1, △ cluster 2, + cluster 3, × cluster 4", pch = 2)

plot(pc.comp1, pc.comp2, pch= clust_df5$Cluster, col=clust_df5$Cluster, main="PAM(K=5, MD)")
points(clust_df5$centers, pch=20)
legend("topright", "cluster 1, △ cluster 2, + cluster 3, × cluster 4, ◇ cluster 5", pch = 2)


#plot(pca)
par(mfrow = c(2, 2))
plot(pc.comp1, pc.comp2, pch= clust_df2$Cluster, col=clust_df2$Cluster, main="PAM(K=2, MD)")
points(clust_df2$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= clust_df3$Cluster, col=clust_df3$Cluster, main="PAM(K=3, MD)")
points(clust_df3$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= clust_df4$Cluster, col=clust_df4$Cluster, main="PAM(K=4, MD)")
points(clust_df4$centers, pch=20)
plot(pc.comp1, pc.comp2, pch= clust_df5$Cluster, col=clust_df5$Cluster, main="PAM(K=5, MD)")
points(clust_df5$centers, pch=20)








#clust_df2$Center <- center.x[clust_df2$Cluster]
#clust_df3$Center <- center.x[clust_df3$Cluster]
#clust_df4$Center <- center.x[clust_df4$Cluster]
#clust_df5$Center <- center.x[clust_df5$Cluster]



names(clust_df2) <- c("객체", "Cluster")
center.x <- main.x[order(main.x)]
clust_df2$Center <- center.x[clust_df2$Cluster]
clust_df3$Cluster
plot(clust_df2$Cluster)














# Cluster Plot against 1st 2 principal components
# http://www.statmethods.net/advstats/cluster.html

clusplot(anal_df, kme_df2$cluster, color=TRUE, shade=TRUE, labels=2, 
         lines=0, cex=.4, xlab="Component 1", ylab="Component 2", main="Cluster Plot against 1st 2 principal components")
clusplot(anal_df, kme_df3$cluster, color=TRUE, shade=TRUE, labels=2, 
         lines=0, cex=.4, xlab="Component 1", ylab="Component 2", main="Cluster Plot against 1st 2 principal components")
clusplot(anal_df, kme_df4$cluster, color=TRUE, shade=TRUE, labels=2, 
         lines=0, cex=.4, xlab="Component 1", ylab="Component 2", main="Cluster Plot against 1st 2 principal components")
clusplot(anal_df, kme_df5$cluster, color=TRUE, shade=TRUE, labels=2, 
         lines=0, cex=.4, xlab="Component 1", ylab="Component 2", main="Cluster Plot against 1st 2 principal components")

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(anal_df, kme_df2$cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df3$cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df4$cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")
plotcluster(anal_df, kme_df5$cluster, cex=.6, main="Centroid Plot against 1st 2 discriminant functions")


###############################################
## 1 ## Connectivity Index - Internal Measure
###############################################
# cluster data , compute connectivity index using data and its clusterization
install.packages("clv")
library(clv)
v2.pred <- as.integer(kme_df2$cluster) # get cluster ids associated to gived data objects
v3.pred <- as.integer(kme_df3$cluster) # get cluster ids associated to gived data objects
v4.pred <- as.integer(kme_df4$cluster) # get cluster ids associated to gived data objects
v5.pred <- as.integer(kme_df5$cluster) # get cluster ids associated to gived data objects

conn2 <- connectivity(anal_df, v2.pred, 10)
conn3 <- connectivity(anal_df, v3.pred, 10)
conn4 <- connectivity(anal_df, v4.pred, 10)
conn5 <- connectivity(anal_df, v5.pred, 10)
conn <- c(conn2,conn3,conn4, conn5)
print(conn)
plot(conn, type="b", xlim=c(1,5))

conn2 <- connectivity(anal_df, v2.pred, 10, dist="correlation")
conn2 <- connectivity(anal_df, v2.pred, 10, dist="manhattan")


# the same using dissimilarity matrix
anal.diss.mx <- as.matrix(daisy(anal_df))
conn2 <- connectivity.diss.mx(anal.diss.mx, v2.pred, 10)

##############################################################
## 2 ##  Total separation between clusters - Internal Measure
##############################################################
# compute Dis index  
# http://rpackages.ianhowson.com/cran/clv/man/Dis.html
scatt2 <- clv.Scatt(anal_df, v2.pred)
dis2 <- clv.Dis(scatt2$cluster.center)

scatt3 <- clv.Scatt(anal_df, v3.pred)
dis3 <- clv.Dis(scatt3$cluster.center)

scatt4 <- clv.Scatt(anal_df, v4.pred)
dis4 <- clv.Dis(scatt4$cluster.center)

scatt5 <- clv.Scatt(anal_df, v5.pred)
dis5 <- clv.Dis(scatt5$cluster.center)

scatt <- c(scatt2$Scatt,scatt3$Scatt,scatt4$Scatt, scatt5$Scatt)
dis <- c(dis2, dis3, dis4, dis5)
print(dis)
plot(dis, type="b",xlim=c(1,5))
plot(scatt, type="b",xlim=c(1,5))

##############################################################
## 3 ## Dunn Index - Internal Measure
##############################################################
#http://rpackages.ianhowson.com/cran/clv/man/Dunn.html

intraclust = c("complete","average","centroid")
interclust = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")

# compute Dunn indicies (also Davies-Bouldin indicies)
# 1. optimal solution:
# compute intercluster distances and intracluster diameters
cls.scatt <- cls.scatt.data(anal_df, v2.pred, dist="euclid")

# once computed valuse use in both functions
dunn1 <- clv.Dunn(cls.scatt, intraclust, interclust)
davies1 <- clv.Davies.Bouldin(cls.scatt, intraclust, interclust)

# 2. functional solution:
# define new Dunn and Davies.Bouldin functions
Dunn <- function(data,clust) 
  clv.Dunn( cls.scatt.data(data,clust),
            intracls = c("complete","average","centroid"), 
            intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )
Davies.Bouldin <- function(data,clust) 
  clv.Davies.Bouldin( cls.scatt.data(data,clust),
                      intracls = c("complete","average","centroid"),
                      intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )

# compute indicies
dunn2 <- Dunn(anal_df, v2.pred)
davies2 <- Davies.Bouldin(anal_df, v2.pred)
dunn3 <- Dunn(anal_df, v3.pred)
davies3 <- Davies.Bouldin(anal_df, v3.pred)
dunn4 <- Dunn(anal_df, v4.pred)
davies4 <- Davies.Bouldin(anal_df, v4.pred)
dunn5 <- Dunn(anal_df, v5.pred)
davies5 <- Davies.Bouldin(anal_df, v5.pred)

plot(c(dunn2[1,1],dunn3[1,1],dunn4[1,1],dunn5[1,1]),xlim = c(1, 4), ylim = c(0,0.8),type="b", ylab="Dunn", xlab="K")
points(c(dunn2[2,1],dunn3[2,1],dunn4[2,1],dunn5[2,1]), type="b",col="red", ylab="Dunn", xlab="K")
points(c(dunn2[3,1],dunn3[3,1],dunn4[3,1],dunn5[3,1]), type="b",col="blue", ylab="Dunn", xlab="K")
points(c(dunn2[4,1],dunn3[4,1],dunn4[4,1],dunn5[4,1]), type="b",col="green", ylab="Dunn", xlab="K")
points(c(dunn2[5,1],dunn3[5,1],dunn4[5,1],dunn5[5,1]), type="b",col="orange", ylab="Dunn", xlab="K")
points(c(dunn2[6,1],dunn3[6,1],dunn4[6,1],dunn5[6,1]), type="b",col="gray", ylab="Dunn", xlab="K")

plot(c(dunn2[1,2],dunn3[1,2],dunn4[1,2],dunn5[1,2]),xlim = c(1, 4), ylim = c(0,4),type="b", ylab="Dunn", xlab="K")
points(c(dunn2[2,2],dunn3[2,2],dunn4[2,2],dunn5[2,2]), type="b",col="red", ylab="Dunn", xlab="K")
points(c(dunn2[3,2],dunn3[3,2],dunn4[3,2],dunn5[3,2]), type="b",col="blue", ylab="Dunn", xlab="K")
points(c(dunn2[4,2],dunn3[4,2],dunn4[4,2],dunn5[4,2]), type="b",col="green", ylab="Dunn", xlab="K")
points(c(dunn2[5,2],dunn3[5,2],dunn4[5,2],dunn5[5,2]), type="b",col="orange", ylab="Dunn", xlab="K")
points(c(dunn2[6,2],dunn3[6,2],dunn4[6,2],dunn5[6,2]), type="b",col="gray", ylab="Dunn", xlab="K")

plot(c(dunn2[1,3],dunn3[1,3],dunn4[1,3],dunn5[1,3]),xlim = c(1, 4), ylim = c(0,6),type="b", ylab="Dunn", xlab="K")
points(c(dunn2[2,3],dunn3[2,3],dunn4[2,3],dunn5[2,3]), type="b",col="red", ylab="Dunn", xlab="K")
points(c(dunn2[3,3],dunn3[3,3],dunn4[3,3],dunn5[3,3]), type="b",col="blue", ylab="Dunn", xlab="K")
points(c(dunn2[4,3],dunn3[4,3],dunn4[4,3],dunn5[4,3]), type="b",col="green", ylab="Dunn", xlab="K")
points(c(dunn2[5,3],dunn3[5,3],dunn4[5,3],dunn5[5,3]), type="b",col="orange", ylab="Dunn", xlab="K")
points(c(dunn2[6,3],dunn3[6,3],dunn4[6,3],dunn5[6,3]), type="b",col="gray", ylab="Dunn", xlab="K")

plot(c(davies2[1,1],davies3[1,1],davies4[1,1],davies5[1,1]),xlim = c(1, 4), ylim = c(0,30),type="b", ylab="davies", xlab="K")
points(c(davies2[2,1],davies3[2,1],davies4[2,1],davies5[2,1]), type="b",col="red", ylab="davies", xlab="K")
points(c(davies2[3,1],davies3[3,1],davies4[3,1],davies5[3,1]), type="b",col="blue", ylab="davies", xlab="K")
points(c(davies2[4,1],davies3[4,1],davies4[4,1],davies5[4,1]), type="b",col="green", ylab="davies", xlab="K")
points(c(davies2[5,1],davies3[5,1],davies4[5,1],davies5[5,1]), type="b",col="orange", ylab="davies", xlab="K")
points(c(davies2[6,1],davies3[6,1],davies4[6,1],davies5[6,1]), type="b",col="gray", ylab="davies", xlab="K")

plot(c(davies2[1,2],davies3[1,2],davies4[1,2],davies5[1,2]),xlim = c(1, 4), ylim = c(0,10),type="b", ylab="davies", xlab="K")
points(c(davies2[2,2],davies3[2,2],davies4[2,2],davies5[2,2]), type="b",col="red", ylab="davies", xlab="K")
points(c(davies2[3,2],davies3[3,2],davies4[3,2],davies5[3,2]), type="b",col="blue", ylab="davies", xlab="K")
points(c(davies2[4,2],davies3[4,2],davies4[4,2],davies5[4,2]), type="b",col="green", ylab="davies", xlab="K")
points(c(davies2[5,2],davies3[5,2],davies4[5,2],davies5[5,2]), type="b",col="orange", ylab="davies", xlab="K")
points(c(davies2[6,2],davies3[6,2],davies4[6,2],davies5[6,2]), type="b",col="gray", ylab="davies", xlab="K")

plot(c(davies2[1,3],davies3[1,3],davies4[1,3],davies5[1,3]),xlim = c(1, 4), ylim = c(0,6),type="b", ylab="davies", xlab="K")
points(c(davies2[2,3],davies3[2,3],davies4[2,3],davies5[2,3]), type="b",col="red", ylab="davies", xlab="K")
points(c(davies2[3,3],davies3[3,3],davies4[3,3],davies5[3,3]), type="b",col="blue", ylab="davies", xlab="K")
points(c(davies2[4,3],davies3[4,3],davies4[4,3],davies5[4,3]), type="b",col="green", ylab="davies", xlab="K")
points(c(davies2[5,3],davies3[5,3],davies4[5,3],davies5[5,3]), type="b",col="orange", ylab="davies", xlab="K")
points(c(davies2[6,3],davies3[6,3],davies4[6,3],davies5[6,3]), type="b",col="gray", ylab="davies", xlab="K")


# Thus, based on the Dunn’s index definition, we may conclude that large values of
# the index indicate the presence of compact and well-separated clusters.


##############################################################
## 4 ##  Matrix Cluster Scatter Measures
##############################################################
#http://rpackages.ianhowson.com/cran/clv/man/wcls_bcls_matrices.html
#Estimating the number of clusters in a data set viathe gap statistic
# compute cluster sizes, center of each cluster and mean from data objects

cls.attr <- cls.attrib(anal_df, v2.pred)
center <- cls.attr$cluster.center
size <- cls.attr$cluster.size
anal.mean <- cls.attr$mean
# compute matrix scatter measures
W.matrix <- wcls.matrix(anal_df, v2.pred, center)
B.matrix <- bcls.matrix(center, size, anal.mean)
T.matrix <- W.matrix + B.matrix
# example of indices based on W, B i T matrices
mx.scatt.crit21 = sum(diag(W.matrix))
mx.scatt.crit22 = sum(diag(B.matrix))/sum(diag(W.matrix))
mx.scatt.crit23 = det(W.matrix)/det(T.matrix)
mx.scatt2 <- c(mx.scatt.crit21 ,mx.scatt.crit22 ,mx.scatt.crit23 )

cls.attr <- cls.attrib(anal_df, v3.pred)
center <- cls.attr$cluster.center
size <- cls.attr$cluster.size
# compute matrix scatter measures
W.matrix <- wcls.matrix(anal_df, v3.pred, center)
B.matrix <- bcls.matrix(center, size, anal.mean)
T.matrix <- W.matrix + B.matrix
# example of indices based on W, B i T matrices
mx.scatt.crit31 = sum(diag(W.matrix))
mx.scatt.crit32 = sum(diag(B.matrix))/sum(diag(W.matrix))
mx.scatt.crit33 = det(W.matrix)/det(T.matrix)
mx.scatt3 <- c(mx.scatt.crit31 ,mx.scatt.crit32 ,mx.scatt.crit33 )

cls.attr <- cls.attrib(anal_df, v4.pred)
center <- cls.attr$cluster.center
size <- cls.attr$cluster.size
# compute matrix scatter measures
W.matrix <- wcls.matrix(anal_df, v4.pred, center)
B.matrix <- bcls.matrix(center, size, anal.mean)
T.matrix <- W.matrix + B.matrix
# example of indices based on W, B i T matrices
mx.scatt.crit41 = sum(diag(W.matrix))
mx.scatt.crit42 = sum(diag(B.matrix))/sum(diag(W.matrix))
mx.scatt.crit43 = det(W.matrix)/det(T.matrix)
mx.scatt4 <- c(mx.scatt.crit41 ,mx.scatt.crit42 ,mx.scatt.crit43 )

cls.attr <- cls.attrib(anal_df, v5.pred)
center <- cls.attr$cluster.center
size <- cls.attr$cluster.size
# compute matrix scatter measures
W.matrix <- wcls.matrix(anal_df, v5.pred, center)
B.matrix <- bcls.matrix(center, size, anal.mean)
T.matrix <- W.matrix + B.matrix
# example of indices based on W, B i T matrices
mx.scatt.crit51 = sum(diag(W.matrix))
mx.scatt.crit52 = sum(diag(B.matrix))/sum(diag(W.matrix))
mx.scatt.crit53 = det(W.matrix)/det(T.matrix)
mx.scatt5 <- c(mx.scatt.crit51 ,mx.scatt.crit52 ,mx.scatt.crit53 )

mx.scatt1 <- c(mx.scatt.crit21,mx.scatt.crit31,mx.scatt.crit41,mx.scatt.crit51)
plot(mx.scatt1, type="b")
mx.scatt2 <- c(mx.scatt.crit22,mx.scatt.crit32,mx.scatt.crit42,mx.scatt.crit52)
plot(mx.scatt2, type="b")
mx.scatt3 <- c(mx.scatt.crit23,mx.scatt.crit33,mx.scatt.crit43,mx.scatt.crit53)
plot(mx.scatt3, type="b", xlim=c(1,5), xlab="The number of clusters(K)" 
     ,ylab="Matrix scatter measures", main="The number of Cluseters")


# comparing 2 cluster solutions
d <- dist(anal_df, method = "euclidean") # distance matrix
stat <- cluster.stats(d, kme_df2$cluster, kme_df3$cluster)

## Model Based Clustering
#library(mclust)
#fit <- Mclust(anal_df)
#plot(fit) # plot results 
#summary(fit) # display the best model

## Scaled data clustering
anal_scaled_df <- scale(anal_df) # standardize variables

## PCA Plot - K-means_ED
pca <- princomp(anal_df, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
X <- cbind(pc.comp1, pc.comp2)
par(mfrow = c(2, 2))
cl2 <- kmeans(X,2)
plot(pc.comp1, pc.comp2, pch= cl2$cluster, col=cl2$cluster, main="K-means(K=2, ED)")
points(cl2$centers, pch=18)
cl3 <- kmeans(X,3)
plot(pc.comp1, pc.comp2, pch= cl3$cluster, col=cl3$cluster, main="K-means(K=3, ED)")
points(cl3$centers, pch=20)
cl4 <- kmeans(X,4)
plot(pc.comp1, pc.comp2, pch= cl4$cluster, col=cl4$cluster, main="K-means(K=4, ED)")
points(cl4$centers, pch=20)
cl5 <- kmeans(X,5)
plot(pc.comp1, pc.comp2, pch= cl5$cluster, col=cl5$cluster, main="K-means(K=5, ED)")
points(cl5$centers, pch=20)









































#=============================================================
# 군집분석 결과 비교(R 내장 함수 vs. 사용자 정의 함수)
# ============================================================
# Mahalanobis 거리계산 function
# 필요 패키지: MASS
# ============================================================

mahal_fn <- function (x, center, cor, inverted = FALSE, ...) 
{
  x <- if(is.vector(x)) 
    matrix(x, ncol = length(x))
  else as.matrix(x)
  x <- sweep(x, 2, center)
  if(!inverted) 
    cor <- ginv(cor, ...)
  setNames(rowSums((x %*% cor) * x), rownames(x))
  
}


# ============================================================
# Mahalanobis Kmeans Clustering function
# 필요 패키지: 없음
# 파라미터
# - anal_df   = 분석 대상 데이터(연속형)
# - cluster.k = 군집 개수 정의
# - init.x    =  1: Clustering 개수만큼 데이터를 추출하여
#                   해당 데이터를 초기 중심값으로 설정
#                2: 데이터를 무작위로 cluster.k 개수 만큼 Grouping하여
#                   각 Group의 중심값(평균)을 초기 중심값으로 설정
# - group     = 유사한 변수 집단 정의(MD에 의한 중요도 계산을 위한)
# - fs.m      = 변수 중요도 평가 옵션
#               "T-TEST" : t-검정에 의한 변수의 중요도 평가
#               "MD"     : MD Silhouett 기법을 적용한 중요도 평가
#               "ALL"    : 방법론 모두 사용
#               "NONE"   : 변수 중요도 평가하지 않음
# ============================================================

mks_fn <- function(anal_df, cluster.k, init.x , group.v, fs.m)
{
  ## clustering 데이터 정의
  cluster_df <- anal_df
  ## 수렴여부 검증 Flag 초기값 설정(0: 미수렴, 1: 수렴)
  check.x <- 0
  ## Iteration 횟수 저장 벡터
  iter.x <- 0
  # ----------------------------------------
  # 초기 중심값 설정
  # ----------------------------------------
  if(init.x == 1) { 
    # Case 1.
    ## Clustering 개수만큼 데이터를 추출하여
    ## 해당 데이터를 초기 중심값으로 설정
    
    ## Clustering 개수만큼 초기 중심값 추출
    length.x  <- c(1:nrow(anal_df))
    center.x  <- sample(length.x, cluster.k)
    init.center <- anal_df[center.x, ]
    
  } else {
    # Case 2.
    ## 데이터를 무작위로 cluster.k 개수 만큼 Grouping하여
    ## 각 Group의 중심값(평균)을 초기 중심값으로 설정
    set.seed(12345)
    group.idx <- rnorm(1:nrow(anal_df))
    tmp_df    <- anal_df[order(group.idx), ]
    tmp_df    <- data.frame(group = rep(1:cluster.k, len = nrow(tmp_df)), tmp_df)
    init.center <- aggregate(. ~ group, data = tmp_df, mean)
    init.center <- init.center[ , -1]
    rm(tmp_df)
    
  }
  # ----------------------------------------
  # 거리계산 Method 정의 - Mahalanobis 거리 계산
  # ----------------------------------------
  ## 거리 계산
  for(k in 1:cluster.k)
  {
    #k <- 1
    tmp_df <- mahal_fn(x = anal_df, 
                       center = as.numeric(init.center[k, ]),
                       cor = cor(anal_df))
    if(k == 1) {
      dist_df <- data.frame(tmp_df)
    } else {
      dist_df <- cbind(dist_df, tmp_df)
    }
  }
  names(dist_df) <- paste("Clust_", 1:cluster.k, sep = "")
  # ----------------------------------------
  # Cluster 할당
  # ----------------------------------------
  clust.x <- apply(dist_df, 1, function(x) which.min(x))
  final_df <- anal_df
  final_df$Cluster <- clust.x
  # ---------------------------------------
  # 오브젝트 제거
  # ----------------------------------------
  rm(dist_df, tmp_df)
  # ============================================================
  # 반복문 수행
  
  while(check.x != 1)
  {
    # ----------------------------------------
    # 새로운 중심값 계산
    # ----------------------------------------
    new.center <- aggregate(. ~ Cluster, data = final_df, mean)
    # ----------------------------------------
    # 거리 재계산
    # ----------------------------------------
    for(k in 1:cluster.k)
    {
      #k <- 1
      tmp_df <- mahal_fn(x = anal_df, 
                         center = as.numeric(new.center[k, -1]),
                         cor = cor(anal_df))
      if(k == 1) {
        dist_df <- data.frame(tmp_df)
      } else {
        dist_df <- cbind(dist_df, tmp_df)
      }
    }
    # ----------------------------------------
    # Cluster 재할당
    # ----------------------------------------
    clust.x <- apply(dist_df, 1, function(x) which.min(x))
    final_df$New_Cluster <- clust.x
    # ----------------------------------------
    # 수렴 여부 체크
    # ----------------------------------------
    if(sum(final_df$New_Cluster != final_df$Cluster) == 0)
    { 
      check.x <- 1
      final_df$Cluster <- final_df$New_Cluster
      final_df <- final_df[, -ncol(final_df)]
    } else {
      check.x <- 0
      final_df$Cluster <- final_df$New_Cluster
      final_df <- final_df[, -ncol(final_df)]
    }
    iter.x <- iter.x + 1
    # ----------------------------------------
    # 오브젝트 제거
    # ----------------------------------------
    rm(dist_df, tmp_df)
  }
  # ============================================================
  # Output 도출
  output_li <- list()
  # ----------------------------------------
  # Cluster 분류결과
  # ----------------------------------------
  output_li[[1]] <- final_df$Cluster
  names(output_li)[[1]] <- "Cluster"
  # ----------------------------------------
  # Cluster Means
  # ----------------------------------------
  output_li[[2]] <- aggregate(. ~ Cluster, data = final_df, mean)
  names(output_li)[[2]] <- "Cluster_Means"
  # ----------------------------------------
  # Iteration
  # ----------------------------------------
  output_li[[3]] <- iter.x
  names(output_li)[[3]] <- "Iteration"
  # ----------------------------------------
  # 내부평가 - Silhouette Technique
  # ----------------------------------------
  s.i <- c()
  for(i in 1:nrow(final_df))
  {
    #i <- 1
    i.x <- final_df[i, ncol(final_df)]
    inclust_df  <- subset(final_df, Cluster == i.x, select = names(anal_df))
    outclust_df <- subset(final_df, Cluster != i.x, select = names(anal_df))
    a.i <- mean(mahal_fn(x = inclust_df, 
                         center = as.numeric(final_df[i, 1:ncol(anal_df)]),
                         cor = cor(inclust_df)))
    b.i <- mean(mahal_fn(x = outclust_df, 
                         center = as.numeric(final_df[i, 1:ncol(anal_df)]),
                         cor = cor(outclust_df)))
    s.i[i] <- (b.i - a.i) / max(a.i, b.i)
  }
  output_li[[4]] <- mean(s.i)
  names(output_li)[[4]] <- "Silhouette"
  
  # ----------------------------------------
  # 변수의 중요도 평가
  # ---------------------------------------
  if(fs.m %in% c("T-TEST", "ALL")) {  
    ## Case 1. t-test
    result_mat <- matrix(NA, nrow = ncol(anal_df), ncol = cluster.k)
    rank_mat   <- matrix(NA, nrow = ncol(anal_df), ncol = cluster.k)
    for(i in 1:cluster.k) {
      test_i_df   <- subset(final_df, Cluster == i, select = names(anal_df))
      test_you_df <- subset(final_df, Cluster != i, select = names(anal_df))
      for(j in 1:ncol(anal_df)) {
        result_mat[j, i] <- t.test(test_i_df[j], test_you_df[j])$p.value      
      }
      rank_mat[ , i] <- rank(result_mat[, i])
    }
    result_mat <- as.data.frame(result_mat) ; rank_mat <- as.data.frame(rank_mat)
    colnames(result_mat) <- c(1:cluster.k) ; colnames(rank_mat) <- c(1:cluster.k)
    rownames(result_mat) <- names(anal_df) ; rownames(rank_mat) <- names(anal_df)
    output_li[[5]] <- result_mat
    output_li[[6]] <- rank_mat
  } else {
    output_li[[5]] <- "해당 옵션을 선택하지 않았습니다."
    output_li[[6]] <- "해당 옵션을 선택하지 않았습니다."
  }
  names(output_li)[[5]] <- "p-value(t-test)"
  names(output_li)[[6]] <- "Importance(p-value)"
  
  if(fs.m %in% c("MD", "ALL")) {
    
    ## Case 2. MD-Silhouette
    var.group <- group.v
    names(var.group) <- names(anal_df)
    mds_mat <- matrix(NA, nrow = max(var.group), ncol = cluster.k)
    tmp.x   <- c()
    for(j in 1:max(var.group)) { 
      #  j <- 1   
      names.x <- names(var.group)[var.group == j]
      tmp_df <- anal_df[ ,names.x]
      for(i in 1:nrow(final_df)) {
        i.x <- final_df[i, ncol(final_df)]
        inclust_df  <- subset(final_df, Cluster == i.x, select = names.x)
        outclust_df <- subset(final_df, Cluster != i.x, select = names.x)
        a.i <- mean(mahal_fn(x = inclust_df, center = as.numeric(tmp_df[i, ]),
                             cor = cor(inclust_df)))
        b.i <- mean(mahal_fn(x = outclust_df, center = as.numeric(tmp_df[i, ]),
                             cor = cor(outclust_df)))
        tmp.x[i] <- (b.i - a.i) / max(a.i, b.i)
      }
      tmp_df <- data.frame(Dist = tmp.x, Cluster = final_df$Cluster)
      agg_df <- aggregate(Dist ~ Cluster, data = tmp_df, mean)
      
      mds_mat[j, ] <- agg_df$Dist
    }
    colnames(mds_mat) <- c(1:cluster.k)
    rownames(mds_mat) <- paste0("Variables_", 1:max(var.group))
    output_li[[7]] <- mds_mat
  } else {
    output_li[[7]] <- "해당 옵션을 선택하지 않았습니다."
  }
  names(output_li)[[7]] <- "Importance(MD)"
  return(output_li)
}

#===========================================================================






s_anal_df <- subset(anal_df, Time_Unit==1|Time_Unit==2,select = -c(TIME_SEQ, YN))
f_anal_df <- subset(anal_df, Time_Unit==10|Time_Unit==11|Time_Unit==12, 
                    select = -c(TIME_SEQ, Time_Unit, YN))  

f_anal_df <- subset(anal_df, Time_Unit==12, 
                    select = -c(TIME_SEQ, Time_Unit, YN))  


#anal_df <- subset(anal_df, X1>(mean(X1)-3*sd(X1))&X1<(mean(X1)+3*sd(X1)))
#names(anal_df)[7] <- "Time_Unit"
anal_df <- subset(anal_df, Time_Unit==1,select = -c(TIME_SEQ, Time_Unit,YN))


anal_df <- subset(anal_df, X1>(mean(X1)-3*sd(X1))&X1<(mean(X1)+3*sd(X1)))
anal_df <- subset(anal_df, X2>(mean(X2)-3*sd(X2))&X2<(mean(X2)+3*sd(X2)))
anal_df <- subset(anal_df, X3>(mean(X3)-3*sd(X3))&X3<(mean(X3)+3*sd(X3)))
anal_df <- subset(anal_df, X4>(mean(X4)-3*sd(X4))&X4<(mean(X4)+3*sd(X4)))
anal_df <- subset(anal_df, X5>(mean(X5)-3*sd(X5))&X5<(mean(X5)+3*sd(X5)))



# Determine number of clusters(1)
#Look for a bend or elbow in the sum of squared error (SSE) scree plot.
wss <- (nrow(anal_df)-1)*sum(apply(anal_df,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(anal_df, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Determine number of clusters(2)- Calinsky criterion
#http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
install.packages("vegan")
require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 100)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!








###########################################
# Cluster Validation
###########################################
#https://cran.r-project.org/web/packages/clValid/vignettes/clValid.pdf

install.packages("clValid")
library(clValid)

#1# Internal Validation
data(mouse)

express <- mouse[,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID

intern <- clValid(express, 2:6, clMethods=c("hierarchical","kmeans","pam"), validation="internal")

summary(intern)

op <- par(no.readonly=TRUE)

par(mfrow=c(2,2),mar=c(4,4,3,1))

plot(intern, legend=FALSE)

plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")

legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))

par(op)


#3# Stability Validation

stab <- clValid(express, 2:6, clMethods=c("hierarchical","kmeans","pam"), validation="stability")
optimalScores(stab)

par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
plot(nClusters(stab),measures(stab,"APN")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#3# Rank Aggregation
install.packages("RankAggreg")
library(RankAggreg)
result <- clValid(express, 4:6, clMethods=c("hierarchical","kmeans","pam"), validation=c("internal","stability"))
res <- getRanksWeights(result)

print(res$ranks[,1:3], quote=FALSE)

if(require("RankAggreg")) {
  CEWS <- RankAggreg(x=res$ranks, k=5, weights=res$weights, seed=123, verbose=FALSE)
  CEWS
}



#4# Cluster Purity

ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

n = 1e6
classes = sample(3, n, replace=T)
clusters = sample(5, n, replace=T)
ClusterPurity(clusters, classes)
[1] 0.334349