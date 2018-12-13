install.packages("car")   # 선형회귀 분석용 함수
install.packages("lme4")  # 그래픽 패키지
install.packages("Rcpp")
install.packages("MatrixModels")
install.packages("lattice")

library(car)
library(lattice)

setwd("D:/work")

# 데이터를 읽어들여 데이터 프레임 "dodgers"에 저장하기
dodgers <- read.csv("./dataset/dodgers.csv")
print(str(dodgers)) # 데이터 프레임을 출력하여 구조를 확인하기

# 플롯과 데이터 요약 결과를 얻기위해 정렬된 요일변수 정의하기 (월=1,화=2,수=3,목=4,금=5,토=6,일=7)
dodgers$ordered_day_of_week <- with(data=dodgers,
                                    ifelse((day_of_week == "Monday"),1,
                                    ifelse((day_of_week == "Tuesday"),2,
                                    ifelse((day_of_week == "Wednesday"),3,
                                    ifelse((day_of_week == "Thursday"),4,
                                    ifelse((day_of_week == "Friday"),5,
                                    ifelse((day_of_week == "Saturday"),6,7)))))))

dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
                                      labels=c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"))

# 표준그래픽으로 탐색적 데이터 분석 : 요일별 관중수
with(data = dodgers, plot(ordered_day_of_week, attend/1000,
                          xlab = "Day of Week", ylab = "Attendance (thousands)", col = "violet", las = 1))

# LA 다져스가 버블헤드 인형으로 홍보하는 시기
with(dodgers, table(bobblehead, ordered_day_of_week)) # 화요일 버블헤드 인형으로 홍보

# 플롯과 데이터 요약 결과를 얻기 위해 정렬된 월변수를 정의하기
dodgers$ordered_month <- with(data = dodgers,
                              ifelse((month == "APR"),4,
                              ifelse((month == "MAY"),5,
                              ifelse((month == "JUN"),6,
                              ifelse((month == "JUL"),7,
                              ifelse((month == "AUG"),8,
                              ifelse((month == "SEP"),9,10)))))))

dodgers$ordered_month <- factor(dodgers$ordered_month, levels = 4:10, labels = c("April","May","June","July","Aug","Sept","Oct"))

# 표준 R 그래픽으로 탐색적 데이터 분석 : 월별 관중수
with(data = dodgers, plot(ordered_month, attend/1000, xlab = "Month", ylab = "Attendance (thousands)", col = "light blue", las = 1))

# 주간/야간 경기 그리고 불꽃놀이의 유무에 따른 많은 변수값을 시각화한 탐색적 데이터 분석
library(lattice) # 플로팅을 위해 사용하는 라이브러리

# LA 다저스 데이터에 대한 그래픽 요약을 실행하기 위한 준비작업
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <-c("black","black")
group.fill <- c("black", "red")
xyplot(attend/1000 ~ temp | skies + day_night , data = dodgers, groups = fireworks, pch = c(group.symbols), 
       aspect = 1, cex = 1.5, col = group.colors, fill = group.fill, layout = c(2,2), type = c("p","g"), strip = strip.custom(strip.levels = TRUE, strip.names = FALSE, style = 1),
       xlab = "Temperature (Degrees Fahrenheit)",
       ylab = "Attendance (thousands)",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                              points = list(pch = rev(group.symbols), col = rev(group.colors), fill = rev(group.fill))))

# 상대팀과 주간/야간 경기에 따른 관중수
group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night,
       xlab = "Attendance (thousands)",
       panel = function(x, y, groups, subscripts, ...)
              {
              panel.grid(h = (length(levels(dodgers$opponent)) - 1), v= -1, identifier = "grid")
              panel.stripplot(x, y, groups = groups, subscripts = 3,
                              cex = group.symbols.size, pch = group.symbols, col = "darkblue")
              },
       key = list(space = "top",
                  text = list(group.labels, col = "black"), 
                  points = list(pch = group.symbols, cex = group.symbols.size, col = "darkblue")))   

# 마지막에 버블헤드 인형변서 bobblehead를 포함시킨 간단한 모형을 만들기
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}

# 훈련-검증방법을 사용
set.seed(1234) # 훈련-검증용 데이터 분할을 여러 번 반복하기 위한 시드설정
training_test <- c(rep(1, length = trunc((2/3) * nrow(dodgers))),
                   rep(2, length = (nrow(dodgers) - trunc((2/3) * nrow(dodgers)))))

dodgers$training_test <- sample(training_test) # 랜덤 순열
dodgers$training_test <- factor(dodgers$training_test, levels = c(1,2), labels = c("TRAIN", "TEST"))

dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # 훈련용 데이터 프레임 확인
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # 검증용 데이터 프레임 확

# 훈련용 데이터 집합을 대상으로 모형 적합
train.model.fit <- lm(my.model, data = dodgers.train)

# 훈련용 데이터 집합을 이용하여 예측값 계산
dodgers.train$predict_attend <- predict(train.model.fit)

# 검증용 데이터 집합을 이용하여 예측값 평가
dodgers.test$predict_attend <- predict(train.model.fit, newdata = dodgers.test)

# 표본으로 예측할 때 설명이 가능한 반응값의 변동비율 계산
cat("\n", "Progortion of Teset Set Variance Accounted for : ", round((with(dodgers.test, cor(attend, predict_attend)^2)), digits = 3), "\n", sep = "")

# 플로팅용 훈련용과 검증용 데이터 집합을 합치기
dodgers.plotting.frame <- rbind(dodgers.train, dodgers.test)

# 경영진에게 시각적으로 설명하기 위한 예측 모델링 생성
group.labels <- c("No Bobbleheads", "Bobbleheads")
group.symbols <- c(21, 24)
group.colors <- c("black", "black")
group.fill <- c("black", "red")
xyplot(predict_attend/1000 ~ attend/1000 | training_test, data = dodgers.plotting.frame, groups = bobblehead, cex = 2, pch = group.symbols,
       col = group.colors, fill = group.fill, layout = c(2,1), xlim = c(20, 65), ylim = c(20, 65), aspect = 1, type = c("p", "g"), 
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.segments(25,25,60,60,col = "black", cex = 2)
        },
       strip = function(...) strip.default(..., style = 1),
       xlab = "Actual Attendance (thousands)",
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list (pch = rev(group.symbols),
                                 col = rev(group.colors),
                                 fill = rev(group.fill))))

# 다른 요인을 통제한 상태에서 버블헤드 인형에 의한 관중수 증가를 추정하기 위해 전체 데이터 집합을 사용
my.model.fit <- lm(my.model, data = dodgers) # 사용 가능한 모든 데이터를 사용
print(summary(my.model.fit))

# 버블헤드 인형을 이용한 홍보활도엥 대한 통계적 유의성 검증
# 유형1 분산분석으로 순차적 검증에 대한 제곱합을 계산

print(anova(my.model.fit))
cat("\n", "Estimated Effect of Bobblehead Promotion on Attendance : ", 
    round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
          digits = 0), "\n", sep = "")

# 표준 그래픽을 사용하여 진단용 플롯 표시
plot(my.model.fit)

# 패키지 car을 이용하여 추가 모형 진단하기
install.packages("car")
library(car)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

# 추가로 해볼 말한 분석작업
# 회귀 분석결과 진단
# 다른 선형 예측모형과 다른 설명변수를 조사하자.
# 변수 변환이 모형개발에 유용한지 확인




