set.seed(1) #���� ����
p=p53DataSet[sample(nrow(p53DataSet),  20), ] #������ ���� ����
# px=t(p[, sample(49, 17)])

res <- c()
for(i in 1:100){
  samp=sample(nrow(p), 17) #17:49�� �� �������� ������
  px=t(p[, samp])
  py=t(p[, -samp])
  
  #���л� ���
  cor_px=cor(px) 
  cor_py=cor(py)
  
  #��� ���κ�
  thr1 <- cor_px[upper.tri(cor_px)]
  thr2 <- cor_py[upper.tri(cor_py)]
  sort(thr1)
  max(thr1)
  
  #threshold ��
  gamma1 <- 1-(1/nrow(cor_px))*norm(cor_px, type="2") #�� �ۼ�Ʈ ���ϸ� 0���� ������
  gamma2 <- 1-(1/nrow(cor_py))*norm(cor_py, type="2")
  alpha1<- ceiling(gamma1*((nrow(cor_px)*(nrow(cor_px)-1))/2)) #��Ŀ��� �� ��° ������ ã��
  alpha2<- ceiling(gamma2*((nrow(cor_py)*(nrow(cor_py)-1))/2))
  
  threshold1=thr1[order(thr1)==alpha1] #�ش� �� ã��
  threshold2=thr2[order(thr2)==alpha2]
  
  #��� ����
  cor_px[abs(cor_px)<threshold1]<-0 #threshold���� ���� �� 0����
  cor_py[abs(cor_py)<threshold2]<-0
  
  #�������� ��
  a=norm(cor_px, type="2")
  b=norm(cor_py, type="2")
  res[i] <- (a-b) 
}

print(res)
#���� �������� ��: 0.637168

#���� �������� ������ ū ���� ����
count <- 0
for(i in 1:100){
  if (res[i]>0.637168){
    result <- res[i]
    count <- count + 1
  }
}

print(count)