install.packages("caret")
library(caret)
set.seed(1712)

# iris 데이터 준비 - train 70%, test 30%
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
#training
#testing

# 표준화
training.data <- scale(training[-5])
summary(training.data)

# 모델링 (3개 군집)
iris.kmeans <- kmeans(training.data[,-5], centers = 3, iter.max = 10000)
iris.kmeans$centers

# 군집 인덱스 저장
cluster_index <- as.data.frame(iris.kmeans$cluster)


# 군집 확인
training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)

table(training$Species, training$cluster)

# 적정 군집 갯수 결정 참고용
install.packages("NbClust")
library(NbClust)

nc <- NbClust(training.data, min.nc = 2, max.nc = 15, method = "kmeans")


par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(training.data)

# 새로운 데이터에 군집 할당
training.data <- as.data.frame(training.data)
modFit <- train(x = training.data[,-5], 
                y = training$cluster,
                method = "rpart")

testing.data <- as.data.frame(scale(testing[-5]))
testClusterPred <- predict(modFit, testing.data) 
table(testClusterPred ,testing$Species)
