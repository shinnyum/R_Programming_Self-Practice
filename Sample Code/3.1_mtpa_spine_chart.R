install.packages("support.CEs")

# ���� ������Ʈ �м�

# spine ��Ʈ�� ���� ����� ���� �Լ�
load(file = "D:/work/Github/mtpa_spine_chart.Rdata")
setwd("D:/work/")

# spine ��Ʈ�� 1�������� 45�� �κа�ġ���� ǥ�� ����
# |part-worth| <= 40�̸� spine ��Ʈ�� ���� ǥ���� �� ����
# |part-worths| > 40�̸� ǥ��ȭ �۾� �Ŀ� ǥ���� �� ����

print.digits <- 2 # spine ��Ʈ���� ��µ� ������ �ڸ��� ����

library(support.CEs)

# �����̸� ���� �������� ��ǰ �������� ���� �����ϱ�
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

print(questionnaire(provider.survey)) # ���並 ���� �����׸� ���� ���

sink("questions_for_survey.txt") # �ܺ� �ؽ�Ʈ ���Ϸ� ��������� ���
questionnaire(provider.survey)
sink()

# ���� �Ӽ� �̸��� �÷����ϱ� ���� ����� ���� �Լ�
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

# ������ ������ �Բ� ������Ʈ ���� ���������� �о����
conjoint.data.frame <- read.csv("D:/work/Github/mds-master/MDS_Chapter_1/mobile_services_ranking.csv")

# ������Ʈ �м��� �ʿ��� ȿ�� �ڵ��� ���� �� ��� �����ϱ�
options(contrasts = c("contr.sum","contr.poly"))

# ��ȿ������ ����Ͽ� �������� ���ս�Ű�� (��ȣ�ۿ� ����)
main.effects.model <- lm(ranking ~ brand + startup + monthly + service + retail + apple + samsung + google, 
                         data = conjoint.data.frame)
print(summary(main.effects.model))

# ������Ʈ �������� �ʿ��� ���ո����� �ֿ� ����Ʈ ��Ҹ� ����
conjoint.results <- main.effects.model[c("contrasts","xlevels","coefficients")]
conjoint.results$attributes <- names(conjoint.results$contrasts)

# ������Ʈ ��� ����Ʈ ������ �κа�ġ�� ����ϰ� ����
part.worths <- conjoint.results$xlevels # list of same structure as xlevels

end.index.for.coefficient <- 1 # ������ �����ϱ� ���� �ּ�ȭ
part.worth.vector <- NULL      # �κ� ��ġ�� ���������� ���

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

# ǥ��ȭ �κ�-��ġ�� ���
standardize <- function(x) {(x - mean(x)) / sd(x)}
conjoint.results$standardized.part.worths <- lapply(conjoint.results$part.worths, standardize)

# ���� �Ӽ��� ���� �κ� ��ġ������ ��� �� ����
part.worth.ranges <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)){
  part.worth.ranges[index.for.attribute] <- dist(range(conjoint.results$part.worths[index.for.attribute]))
  dist(range(conjoint.results$part.worths[index.for.attribute]))
}

conjoint.results$part.worths.ranges <- part.worth.ranges
sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))

# ���� �Ӽ��� ���� �߿䵵 ��� �� ����
attribute.importance <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)){
  attribute.importance[index.for.attribute] <- (dist(range(conjoint.results$part.worths[index.for.attribute]))/
                                                  sum.part.worth.ranges) * 100
}

conjoint.results$attribute.importance <- attribute.importance

# �Ӽ����� ������ �����ϱ� ���� ������ ������
attribute.name <- names(conjoint.results$contrasts)
attribute.importance <- as.numeric(attribute.importance)
temp.frame <- data.frame(attribute.name, attribute.importance)
conjoint.results$ordered.attributes <- as.character(temp.frame[sort.list(temp.frame$attribute.importance, decreasing = TRUE), "attribute.name"])

# ����Ʈ ������ �߰��� �������� ���� �ϰ���
conjoint.results$internal.consistency <- summary(main.effects.model)$r.squared

# ������Ʈ �������� ����ϱ� ���� ����� �����Լ�
if(print.digits == 2)
  pretty.print <- function(x){sprintf("%1.2f", round(x,digits = 2))}
if(print.digits == 3)
  pretty.print <- function(x){sprintf("%1.3f", round(x,digits = 3))}

# �ֿܼ� ������Ʈ �������� ���
# ����� ������ ��� ������ �����ϱ� ���� pretty.print�� ���
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

# spine ��Ʈ�� �÷��� ����
# ��� �׷��� ��°���� �ܺ� pdf ���Ϸ� ��µ�
pdf(file = "fig_preference_mobile_services_results.pdf", width = 8.5, height = 11)
spine.chart(conjoint.results)
dev.off() # �׷��� ��� ����̽��� �ݱ�

# �߰��� �غ����� �м��۾�
# �����Ϳ� ���ս�Ų ������ ���� ��ȿ�� �������� �˾Ƶ���.
# �ٸ� ������ �𵨿��� � ���� �ִ°� �˾ƺ���.
# ���� �������� �Ӽ��� �� ��ȣ�ۿ��� �ִ°� �˾ƺ���.

