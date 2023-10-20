calcul_fun(px, "2")
calcul_fun(px, "F")
calcul_fun <- function(px, norm_type){
  # 상관 행렬
  cor_px=cor(px) 
  # cor_py=cor(py)
  
  # 전체 중 몇 퍼센트
  thr1 <- cor_px[upper.tri(cor_px)]
  # thr2 <- cor_py[upper.tri(cor_py)]
  sort(thr1)
  max(thr1)
  
  # threshold 값
  gamma1 <- 1-(1/nrow(cor_px))*norm(cor_px, type="2")
  # gamma2 <- 1-(1/nrow(cor_py))*norm(cor_py, type="2")
  
  ### CHECK
  # gamma1 * nrow(cor_px) * (nrow(cor_px) - 1) / 2
  # thr1[order(thr1)[1:141]] <- 0
  # 
  # head(thr1, 20)
  # 
  # a <- c(1, 3, 4, 2)
  # sort(a)
  # order(a)
  # a[order(a)]
  
  alpha1 <- gamma1*max(thr1) # 이거 맞는지 확인,,
  # alpha2 <- gamma2*max(thr2)
  
  # 행렬 수정
  cor_px[abs(cor_px)<alpha1]<-0
  # cor_py[abs(cor_py)<alpha2]<-0
  
  # 고유값 차
  a=norm(cor_px, type=norm_type)
  # b=norm(cor_py, type="2")
  
  return(list(eval_x = a,
              alpha1 = alpha1))
}




set.seed(1) # 씨드 고정
p=p53DataSet[sample(nrow(p53DataSet),  20), ]
# px=t(p[, sample(49, 17)])
i=1
for(i in 1:100){
  samp=sample(nrow(p), 17)
  # px=t(p[, samp])
  # py=t(p[, -samp])
  res_eigenvalue <- calcul_fun(px = t(p[, samp]), 
                               py = t(p[, -samp]))
  res[i] <- (a-b)
}

print(res)
# 실제 두 집단의 고유값 차: 0.637168



user_fun <- function(px, py){
  a <- px + py
  b <- px - py
  return(list(res_a = a,
              res_b = b))
}

res_fun <- user_fun(3, 4)
res_fun$res_a
res_fun$res_b
res_fun[[1]]
res_fun[[2]]


for(i in )
