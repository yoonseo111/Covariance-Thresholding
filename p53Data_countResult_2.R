set.seed(1) # 씨드 고정
p=p53DataSet[sample(nrow(p53DataSet),  20), ]# 20개의 행만 추출
head(p)
p_x=t(p[, (1:17)]) # 집단1
p_y=t(p[, (18:49)]) # 집단2


cor_px=cor(p_x) # 상관관계 matrix
cor_py=cor(p_y) # 상관관계 matrix

# 상관관계 matrix에서 절댓값이 0.2 이하인 값 죽임
cor_px[abs(cor_px)<0.2]<-0
cor_px
cor_py[abs(cor_py)<0.2]<-0
cor_py
cor_py[, (1:5)]

# 고유값 찾기
a=norm(cor_px, type="2")
b=norm(cor_py, type="2")

# 고유값 차
norm(cor_px, type="2")-norm(cor_py, type="2")
a-b

nrow(p53DataSet)
