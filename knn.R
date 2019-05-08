library(tidyverse)
m_df = read.csv('ml_data.csv')

#----data formating-----
m_df = m_df[-1]
m_df$year = as.factor(m_df$year)
m_df$ownerCount = as.factor(m_df$ownerCount)
num.vars = sapply(m_df,is.numeric)
m_df_n <- lapply(m_df[num.vars], scale)
m_df_n = data.frame(m_df_n)
#m_df1 = cbind(hotornot=m_df$hotornot,m_df_n)
str(m_df1)

#----Creating training and test data sets----
set.seed(1)
test = 1:100
train.m_df = m_df_n[-test,]
test.m_df = m_df_n[test,]

train.hot = m_df1$hotornot[-test]
test.hot = m_df1$hotornot[test]

library(class)
knn.1 = knn(train.m_df,test.m_df,train.hot,k=1)
knn.2 = knn(train.m_df,test.m_df,train.hot,k=2)
knn.3 = knn(train.m_df,test.m_df,train.hot,k=3)
knn.5 = knn(train.m_df,test.m_df,train.hot,k=5)
knn.10 = knn(train.m_df,test.m_df,train.hot,k=10)
knn.20 = knn(train.m_df,test.m_df,train.hot,k=20)
knn.24 = knn(train.m_df,test.m_df,train.hot,k=24)
knn.27 = knn(train.m_df,test.m_df,train.hot,k=27)


sqrt(nrow(train.m_df))
100 * sum(test.hot == knn.1)/100
100 * sum(test.hot == knn.2)/100
100 * sum(test.hot == knn.3)/100
100 * sum(test.hot == knn.5)/100
100 * sum(test.hot == knn.10)/100
100 * sum(test.hot == knn.20)/100
100 * sum(test.hot == knn.24)/100
100 * sum(test.hot == knn.27)/100

table(knn.1, test.hot)
table(knn.2, test.hot)
table(knn.3, test.hot)
table(knn.5, test.hot)
table(knn.10, test.hot)
table(knn.20, test.hot)
table(knn.24, test.hot)
table(knn.27, test.hot)


