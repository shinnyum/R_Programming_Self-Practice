# 시장바구니 분석에 대한 연관성 규칙

install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

library(arules) # 연관성 규칙 분석을 위한 패키지
library(arulesViz) # 연관성 규칙에 대한 데이터 시각화를 위한 패키지
library(RColorBrewer) # 그래프의 색상 설정을 위한 팔레트 패키지

data(Groceries) # arules 패키지의 식료품 거래 개체

# 거래 개체의 차원 표시
print(dim(Groceries))
print(dim(Groceries)[1]) # 9835개의 시장바구니
print(dim(Groceries)[2]) # 169개의 초기 식료품 판매물품

# 지지도가 0.025 이상인 항목에 대한 빈도를 계산
pdf(file = "fig_market_basket_initial_item_support.pdf", width = 8.5, height = 11)

itemFrequencyPlot(Groceries, support = 0.025, cex.names = 0.8, xlim = c(0,0.3), type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item", "\n(Item Relative Frequency or Support)"))
dev.off()

# 상호 연관성이 높은 항목 탐색
print(head(itemInfo(Groceries)))
print(levels(itemInfo(Groceries[["level1"]]))) # 10개의 수준, 너무 적음
print(levels(itemInfo(Groceries[["level2"]]))) # 55개의 개별 수준

# 좀 더 의미 있는 항목 집합을 생성하기 위해
# 음식 카테고리에 대해 55개의 level2 수준을 이용하여 항목 집계
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])

print(dim(groceries[1])) # 9835개의 시장바구니 데이터 개수 출력
print(dim(groceries[2])) # 55개의 최종 상점 판매물품(카테고리) 개수 출력

pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.025, cex.names = 1.0, xlim = c(0, 0.5), type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item", "\n(Item Relative Frequency or Support"))
dev.off()

# 카테고리별 판매물품과 모든 구매자간 연관성 분석 결과(규칙)을 구하기
# 매우 낮은 지지도와 신뢰도 기준을 설정하여 실행함
first.rules <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules)) # yields 69,921 rules... too many

# 지지도와 신뢰도에 대한 임계범위를 설정하여 연관규칙을 선정
second.rules <- apriori(groceries, parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules)) # 344개의 규칙을 생성

# 연관규칙을 산점도 형태로 시각화하기
pdf(file = "fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(second.rules, control = list(jitter = 2, col = rev(brewer.pal(9, "Greens")[4:9])), shading = "lift")
dev.off()

# 그룹화한 규칙행렬
pdf(file = "fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(second.rules, method = "grouped", control = list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()

# 연계(오른쪽) 항에 "vegetables"를 갖는 규칙을 선택하기
vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")
inspect(vegie.rules) # 41 rules

# 향상도 기준으로 정렬한 다음 사우이 10개 규칙을 찾아내기
top.vegie.rules <- head(sort(vegie.rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.vegie.rules)

pdf(file = "fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)
plot(top.vegie.rules, method = "graph", control = list(type = "items"), shading = "lift")
dev.off()

# 추가로 해볼만한 분석작업
# 고객대상에서 지역 농부, 축산업 종사자, 낙농업 종사자, 주류업자를 제외한 다음
# 시장바구니 모형으로 고개그이 제품과 관련된 연관성 규칙을 결정하자.
# 앞으로 시장에서 어떻나 제품들이 상호 연계되어 판매될 거신가 알아보자.

