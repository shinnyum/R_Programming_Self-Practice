# ����ٱ��� �м��� ���� ������ ��Ģ

install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

library(arules) # ������ ��Ģ �м��� ���� ��Ű��
library(arulesViz) # ������ ��Ģ�� ���� ������ �ð�ȭ�� ���� ��Ű��
library(RColorBrewer) # �׷����� ���� ������ ���� �ȷ�Ʈ ��Ű��

data(Groceries) # arules ��Ű���� �ķ�ǰ �ŷ� ��ü

# �ŷ� ��ü�� ���� ǥ��
print(dim(Groceries))
print(dim(Groceries)[1]) # 9835���� ����ٱ���
print(dim(Groceries)[2]) # 169���� �ʱ� �ķ�ǰ �ǸŹ�ǰ

# �������� 0.025 �̻��� �׸� ���� �󵵸� ���
pdf(file = "fig_market_basket_initial_item_support.pdf", width = 8.5, height = 11)

itemFrequencyPlot(Groceries, support = 0.025, cex.names = 0.8, xlim = c(0,0.3), type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item", "\n(Item Relative Frequency or Support)"))
dev.off()

# ��ȣ �������� ���� �׸� Ž��
print(head(itemInfo(Groceries)))
print(levels(itemInfo(Groceries[["level1"]]))) # 10���� ����, �ʹ� ����
print(levels(itemInfo(Groceries[["level2"]]))) # 55���� ���� ����

# �� �� �ǹ� �ִ� �׸� ������ �����ϱ� ����
# ���� ī�װ����� ���� 55���� level2 ������ �̿��Ͽ� �׸� ����
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])

print(dim(groceries[1])) # 9835���� ����ٱ��� ������ ���� ���
print(dim(groceries[2])) # 55���� ���� ���� �ǸŹ�ǰ(ī�װ���) ���� ���

pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.025, cex.names = 1.0, xlim = c(0, 0.5), type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item", "\n(Item Relative Frequency or Support"))
dev.off()

# ī�װ����� �ǸŹ�ǰ�� ��� �����ڰ� ������ �м� ���(��Ģ)�� ���ϱ�
# �ſ� ���� �������� �ŷڵ� ������ �����Ͽ� ������
first.rules <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules)) # yields 69,921 rules... too many

# �������� �ŷڵ��� ���� �Ӱ������ �����Ͽ� ������Ģ�� ����
second.rules <- apriori(groceries, parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules)) # 344���� ��Ģ�� ����

# ������Ģ�� ������ ���·� �ð�ȭ�ϱ�
pdf(file = "fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(second.rules, control = list(jitter = 2, col = rev(brewer.pal(9, "Greens")[4:9])), shading = "lift")
dev.off()

# �׷�ȭ�� ��Ģ���
pdf(file = "fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(second.rules, method = "grouped", control = list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()

# ����(������) �׿� "vegetables"�� ���� ��Ģ�� �����ϱ�
vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")
inspect(vegie.rules) # 41 rules

# ��� �������� ������ ���� ����� 10�� ��Ģ�� ã�Ƴ���
top.vegie.rules <- head(sort(vegie.rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.vegie.rules)

pdf(file = "fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)
plot(top.vegie.rules, method = "graph", control = list(type = "items"), shading = "lift")
dev.off()

# �߰��� �غ����� �м��۾�
# ������󿡼� ���� ���, ���� ������, ����� ������, �ַ����ڸ� ������ ����
# ����ٱ��� �������� �������� ��ǰ�� ���õ� ������ ��Ģ�� ��������.
# ������ ���忡�� ��� ��ǰ���� ��ȣ ����Ǿ� �Ǹŵ� �ŽŰ� �˾ƺ���.
