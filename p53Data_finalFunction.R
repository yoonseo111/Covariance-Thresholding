if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GSAR")


library(GSAR)

data("p53DataSet")

# 행 200개 랜덤추출
p=p53DataSet[sample(nrow(p53DataSet),  200), ]
p_x=t(p53DataSet[, (1:17)])
p_x
p_y=t(p53DataSet[, (18:49)]) 
p_y

# 상관계수 matrix
cor_px=cor(p_x)
cor_py=cor(p_y)
cor_px[cor_px<0.2]<-0
cor_px
cor_py[cor_py<0.2]<-0
cor_py

# 행렬의 고유값 찾기
norm(cor_px, type="2")
norm(cor_py, type="2")

# 고유값 차 구하기
norm(cor_px, type="2")-norm(cor_py, type="2")

z <- 10
user_fun <- function(x, y){
  A <- cor(x)
  B <- cor(y)
  
  res <- A[1,2] - B[1,2] + z
  return(res)
}

user_fun(x = matrix(rnorm(50), 10, 5),
         y = matrix(rnorm(50), 10, 5))
