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

# data matrix 추출하기
x=p53DataSet[c(1:200),c(1:17)]
y=p53DataSet[c(1:200), c(18:49)]
x # WT column의 p 200개 추출
y # MUT column의 p 200개 추출

# correlation matrix
cor_x=cor(x)
cor_y=cor(y)

# 특정값 이하 0으로 만들기
cor_x[cor_x<0.2]<-0
cor_x
cor_y[cor_y<0.2]<-0
cor_y

# 최대 고유값 구하기
norm(cor_x, type="2")
norm(cor_y, type="2")

# Z값 구하기
norm(cor_x, type="2")-norm(cor_y, type="2")
