set.seed(1) 
p=p53DataSet[sample(nrow(p53DataSet),  20), ] 
# px=t(p[, sample(49, 17)])
num_sampling <- 17

for(i in 1:100){
  
  #samplingIntoTwo
  
  cor_matrix <- samplingIntoTwo(p, num_sampling)
  cor_px <- cor_matrix$cor_px
  cor_py <- cor_matrix$cor_py
  # 
  # cor_px <- cor(px)
  # cor_py <- cor(py)
  # 
  #calculateThreshold
  threshold1 <- calculateThreshold(cor_px)
  threshold2 <- calculateThreshold(cor_py)
  
  
  cor_px[abs(cor_px)<threshold1]<-0 #threshold
  cor_py[abs(cor_py)<threshold2]<-0
  
  
  a=norm(cor_px, type="2")
  b=norm(cor_py, type="2")
  res[i] <- (a-b) 
}

print(res)
sum(res > 0.637)



count <- 0
for(i in 1:100){
  if (res[i]>0.637168){
    result <- res[i]
    count <- count + 1
  }
}

print(count)
