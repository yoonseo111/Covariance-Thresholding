set.seed(1) # ���� ����
p=p53DataSet[sample(nrow(p53DataSet),  20), ]# 20���� �ุ ����
head(p)
p_x=t(p[, (1:17)]) # ����1
p_y=t(p[, (18:49)]) # ����2


cor_px=cor(p_x) # ������� matrix
cor_py=cor(p_y) # ������� matrix

# ������� matrix���� ������ 0.2 ������ �� ����
cor_px[abs(cor_px)<0.2]<-0
cor_px
cor_py[abs(cor_py)<0.2]<-0
cor_py
cor_py[, (1:5)]

# ������ ã��
a=norm(cor_px, type="2")
b=norm(cor_py, type="2")

# ������ ��
norm(cor_px, type="2")-norm(cor_py, type="2")
a-b

nrow(p53DataSet)