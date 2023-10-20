if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GSAR")


library(GSAR)

data("p53DataSet")
dim(p53DataSet)
?norm
str(p53DataSet)
colnames(p53DataSet)
head(p53DataSet)

# data matrix �����ϱ�
x=p53DataSet[c(1:200),c(1:17)]
y=p53DataSet[c(1:200), c(18:49)]
x # WT column�� p 200�� ����
y # MUT column�� p 200�� ����

# correlation matrix
cor_x=cor(x)
cor_y=cor(y)

# Ư���� ���� 0���� �����
cor_x[cor_x<0.2]<-0
cor_x
cor_y[cor_y<0.2]<-0
cor_y

# �ִ� ������ ���ϱ�
norm(cor_x, type="2")
norm(cor_y, type="2")

# Z�� ���ϱ�
norm(cor_x, type="2")-norm(cor_y, type="2")