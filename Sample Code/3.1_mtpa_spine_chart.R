install.packages("support.CEs")

# 기존 컨조인트 분석

# spine 차트에 대한 사용자 정의 함수
load(file = "D:/work/Github/mtpa_spine_chart.Rdata")
setwd("D:/work/")

# spine 차트는 1페이지에 45개 부분가치까지 표현 가능
# |part-worth| <= 40이면 spine 차트에 직접 표시할 수 있음
# |part-worths| > 40이면 표준화 작업 후에 표시할 수 있음

print.digits <- 2 # spine 차트에서 출력될 숫자의 자리수 설정

library(support.CEs)

# 서베이를 위해 균형잡힌 제품 프로파일 집합 생성하기
provider.survey <- Lma.design(attribute.names = 
                                list(brand = c("AT&T", "T-Mobile", "US Celllular", "Verizon"),
                                     startup = c("$100", "$200", "$300", "$400"),
                                     monthly = c("$100", "$200", "$300", "$400"),
                                     service = c("4G NO", "4G YES"),
                                     retail = c("Retail NO", "Retail YES"),
                                     apple = c("Apple NO", "Apple YES"),
                                     samsung = c("Samsung NO", "Samsung YES"),
                                     google = c("Nexus NO",  "Nexus YES")),
                              nalternatives = 1, nblocks = 1, seed = 9999)

print(questionnaire(provider.survey)) # 검토를 위해 조사항목 설계 출력

sink("questions_for_survey.txt") # 외부 텍스트 파일로 조사사항을 출력
questionnaire(provider.survey)
sink()

# 설명 속성 이름을 플로팅하기 위한 사용자 정의 함수
effect.name.map <- function(effect.name){
  if(effect.name == "brand") return("Mobile Service Provider")
  if(effect.name == "startup") return("Start-up Cost")
  if(effect.name == "monthly") return("Monthly Cost")
  if(effect.name == "service") return("Offers 4G Service")
  if(effect.name == "retail") return("Has nearby Retail Store")
  if(effect.name == "apple") return("Sells Apple Products")
  if(effect.name == "samsung") return("Sells Samsung Products")
  if(effect.name == "google") return("Sells Google/Nexus Products")
}

# 응답결과 순위와 함께 컨조인트 조사 프로파일을 읽어오기
conjoint.data.frame <- read.csv("D:/work/Github/mds-master/MDS_Chapter_1/mobile_services_ranking.csv")

# 컨조인트 분석에 필요한 효과 코딩을 위해 합 대비 설정하기
options(contrasts = c("contr.sum","contr.poly"))

# 주효과만을 사용하여 선형모형 적합시키기 (교호작용 없음)
main.effects.model <- lm(ranking ~ brand + startup + monthly + service + retail + apple + samsung + google, 
                         data = conjoint.data.frame)
print(summary(main.effects.model))

# 컨조인트 측정값에 필요한 적합모형의 주요 리스트 요소를 저장
conjoint.results <- main.effects.model[c("contrasts","xlevels","coefficients")]
conjoint.results$attributes <- names(conjoint.results$contrasts)

# 컨조인트 결과 리스트 구조로 부분가치를 계산하고 저장
part.worths <- conjoint.results$xlevels # list of same structure as xlevels

end.index.for.coefficient <- 1 # 절편을 생략하기 위해 최소화
part.worth.vector <- NULL      # 부분 가치의 축적값으로 사용

for(index.for.attribute in seq(along=conjoint.results$contrasts)){
  nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
  begin.index.for.coefficient <- end.index.for.coefficient + 1
  end.index.for.coefficient <- begin.index.for.coefficient + nlevels -2
  
  last.part.worth <- -sum(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient])
  part.worths[index.for.attribute] <- list(as.numeric(c(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient],
                                                      last.part.worth)))
  
  part.worth.vector <- c(part.worth.vector, unlist(part.worths[index.for.attribute]))
}

conjoint.results$part.worths <- part.worths

# 표준화 부분-가치를 계산
standardize <- function(x) {(x - mean(x)) / sd(x)}
conjoint.results$standardized.part.worths <- lapply(conjoint.results$part.worths, standardize)

# 개별 속성에 대한 부분 가치범위를 계산 및 저장
part.worth.ranges <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)){
  part.worth.ranges[index.for.attribute] <- dist(range(conjoint.results$part.worths[index.for.attribute]))
  dist(range(conjoint.results$part.worths[index.for.attribute]))
}

conjoint.results$part.worths.ranges <- part.worth.ranges
sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))

# 개별 속성에 대한 중요도 계산 및 저장
attribute.importance <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)){
  attribute.importance[index.for.attribute] <- (dist(range(conjoint.results$part.worths[index.for.attribute]))/
                                                  sum.part.worth.ranges) * 100
}

conjoint.results$attribute.importance <- attribute.importance

# 속성명을 순서로 나열하기 위한 데이터 프레임
attribute.name <- names(conjoint.results$contrasts)
attribute.importance <- as.numeric(attribute.importance)
temp.frame <- data.frame(attribute.name, attribute.importance)
conjoint.results$ordered.attributes <- as.character(temp.frame[sort.list(temp.frame$attribute.importance, decreasing = TRUE), "attribute.name"])

# 리스트 구조에 추가된 응답자의 내부 일관성
conjoint.results$internal.consistency <- summary(main.effects.model)$r.squared

# 컨조인트 측정값을 출력하기 위한 사용자 정의함수
if(print.digits == 2)
  pretty.print <- function(x){sprintf("%1.2f", round(x,digits = 2))}
if(print.digits == 3)
  pretty.print <- function(x){sprintf("%1.3f", round(x,digits = 3))}

# 콘솔에 컨조인트 측정값을 출력
# 깔끔한 형태의 출력 포맷을 제공하기 위해 pretty.print를 사용
for(k in seq(along = conjoint.results$ordered.attributes)){
  cat("\n", "\n")
  cat(conjoint.results$ordered.attributes[k], "Levels: ",
      unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))
  
  cat("\n", " Part-Worths: ")
  cat(pretty.print(unlist(conjoint.results$part.worths[conjoint.results$ordered.attributes[k]])))
  
  cat("\n", " Standardized Part-Worths: ")
  cat(pretty.print(unlist(conjoint.results$standardized.part.worths[conjoint.results$ordered.attributes[k]])))
  
  cat("\n", " Attribute Importance: ")
  cat(pretty.print(unlist(conjoint.results$attribute.importance[conjoint.results$ordered.attributes[k]])))
}

# spine 차트의 플로팅 시작
# 모든 그래픽 출력결과는 외부 pdf 파일로 출력됨
pdf(file = "fig_preference_mobile_services_results.pdf", width = 8.5, height = 11)
spine.chart(conjoint.results)
dev.off() # 그래픽 출력 디바이스를 닫기

# 추가로 해볼만한 분석작업
# 데이터에 적합시킨 모형은 선형 주효과 모형임을 알아두자.
# 다른 가능한 모델에는 어떤 것이 있는가 알아보자.
# 서비스 제공자의 속성들 간 교호작용이 있는가 알아보자.


