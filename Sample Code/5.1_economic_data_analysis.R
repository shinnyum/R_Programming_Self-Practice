# 경제 시계열 데이터 분석하기
library(quantmod) # 경제 데이터를 수지밯고 차트화 하기 위한 패키지
library(lubridate) # 날짜 함수 패키지
library(latticeExtra) # 수평 플롯에 사용하는 패키지
library(forecast) # 시계열 예측을 위한 함수 패키지
library(lmtest) # 우연성에 대한 Granger 검정 패키지

par(mfrow = c(2,2)) # 페이지당 한 개의 윈도우에 4개의 플롯을 출력

# St. Louis 연방준비은행의 경제 데이터 (FRED 시스템)
# 실직율(월별, 단위 : 퍼센트)
getSymbols("UNRATENSA", src="FRED", return.class = "xts")
ER <- 100 - UNRATENSA # 실직률로 변환
dimnames(ER)[2] <- "ER"
chartSeries(ER, theme = "white")
ER.data.frame <- as.data.frame(ER)
ER.data.frame$date <- ymd