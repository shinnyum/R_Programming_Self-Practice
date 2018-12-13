# ���� �ð迭 ������ �м��ϱ�
library(quantmod) # ���� �����͸� �����W�� ��Ʈȭ �ϱ� ���� ��Ű��
library(lubridate) # ��¥ �Լ� ��Ű��
library(latticeExtra) # ���� �÷Կ� ����ϴ� ��Ű��
library(forecast) # �ð迭 ������ ���� �Լ� ��Ű��
library(lmtest) # �쿬���� ���� Granger ���� ��Ű��

par(mfrow = c(2,2)) # �������� �� ���� �����쿡 4���� �÷��� ���

# St. Louis �����غ������� ���� ������ (FRED �ý���)
# ������(����, ���� : �ۼ�Ʈ)
getSymbols("UNRATENSA", src="FRED", return.class = "xts")
ER <- 100 - UNRATENSA # �������� ��ȯ
dimnames(ER)[2] <- "ER"
chartSeries(ER, theme = "white")
ER.data.frame <- as.data.frame(ER)
ER.data.frame$date <- ymd