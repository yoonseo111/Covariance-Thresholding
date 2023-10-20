set.seed(1) #씨드 고정
p=p53DataSet[sample(nrow(p53DataSet),  20), ] #데이터 랜덤 추출
# px=t(p[, sample(49, 17)])

res <- c()
for(i in 1:100){
  samp=sample(nrow(p), 17) #17:49로 두 집단으로 나누기
  px=t(p[, samp])
  py=t(p[, -samp])
  
  #공분산 행렬
  cor_px=cor(px) 
  cor_py=cor(py)
  
  #행렬 윗부분
  thr1 <- cor_px[upper.tri(cor_px)]
  thr2 <- cor_py[upper.tri(cor_py)]
  sort(thr1)
  max(thr1)
  
  #threshold 값
  gamma1 <- 1-(1/nrow(cor_px))*norm(cor_px, type="2") #몇 퍼센트 이하를 0으로 만들지
  gamma2 <- 1-(1/nrow(cor_py))*norm(cor_py, type="2")
  alpha1<- ceiling(gamma1*((nrow(cor_px)*(nrow(cor_px)-1))/2)) #행렬에서 몇 번째 값인지 찾기
  alpha2<- ceiling(gamma2*((nrow(cor_py)*(nrow(cor_py)-1))/2))
  
  threshold1=thr1[order(thr1)==alpha1] #해당 값 찾기
  threshold2=thr2[order(thr2)==alpha2]
  
  #행렬 수정
  cor_px[abs(cor_px)<threshold1]<-0 #threshold보다 작은 값 0으로
  cor_py[abs(cor_py)<threshold2]<-0
  
  #고유값의 차
  a=norm(cor_px, type="2")
  b=norm(cor_py, type="2")
  res[i] <- (a-b) 
}

print(res)
#실제 고유값의 차: 0.637168

#실제 고유값의 차보다 큰 값의 개수
count <- 0
for(i in 1:100){
  if (res[i]>0.637168){
    result <- res[i]
    count <- count + 1
  }
}

print(count)
