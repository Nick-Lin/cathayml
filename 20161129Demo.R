## 讀取資料
library(C50)
data(churn)
str(churnTrain)
churnTrain <- churnTrain[,!names(churnTrain)%in%c("state", "area_code", "account_length")]

## 把資料分成訓練與測試集

a <- c(1,2,3,4,5,6,7)
ind <- sample(2, 7, replace=TRUE)
ind
a[ind == 1]
a[ind == 2]

set.seed(2)
ind      <-sample(2, nrow(churnTrain), replace=TRUE, prob=c(0.7, 0.3))
trainset <- churnTrain[ind==1,]
testset  <- churnTrain[ind==2,]

## 建立分類樹
library(rpart)
churn.rp<-rpart(churn ~., data=trainset)
plot(churn.rp, margin=0.1)
text(churn.rp)

library(party)
churn.ct<-ctree(churn ~., data=trainset)
plot(churn.ct)

## 進行剪枝(pruning)
churn.rp$cptable
min(churn.rp$cptable[, 'xerror'])
which.min(churn.rp$cptable[, 'xerror'])
churn.cp <- churn.rp$cptable[7,"CP"]
prune.tree <- prune(churn.rp, cp=churn.cp)
plot(prune.tree, margin = 0.1)
text(prune.tree)

## 預測結果
head(predict(prune.tree, testset))
predictions <- predict(prune.tree, testset, type='class')
length(predictions)
table(testset$churn, predictions)

(95 + 863) / 1018

