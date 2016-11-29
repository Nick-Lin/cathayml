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


library(caret)
cm <- confusionMatrix(table(testset$churn, predictions))
cm$overall[1]

## DIY cross validation　
res <- data.frame()
set.seed(2)
ind <- sample(10, nrow(churnTrain), replace=TRUE)
for (i in seq(1,10)){
  trainset <- churnTrain[ind != i, ]
  testset  <- churnTrain[ind == i, ]
  fit <- rpart(churn ~., trainset)
  predictions <- predict(fit, testset, type= 'class')
  cm <- confusionMatrix(table(testset$churn, predictions))
  res <- rbind(res, cm$overall)
}

## Use caret 進行K-fold cross-validation
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model   <- train(churn~., data=trainset, method="rpart", preProcess="scale", trControl=control)
model

## 找出最重要的變數
library(rminer)
model=fit(churn~.,trainset,model="svm")
VariableImportance=Importance(model,trainset,method="sensv")
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(trainset),col="gray",Grid=10)


## 尋找高相關性變數
new_train <- trainset[,!names(churnTrain)%in%c("churn", "international_plan", "voice_mail_plan")]
cor_mat   <- cor(new_train)
highlyCorrelated <- findCorrelation(cor_mat, cutoff=0.75)
names(new_train)[highlyCorrelated]


## ROC

set.seed(2)
ind      <-sample(2, nrow(churnTrain), replace=TRUE, prob=c(0.7, 0.3))
trainset <- churnTrain[ind==1,]
testset  <- churnTrain[ind==2,]

library(rpart)
churn.rp<-rpart(churn ~., data=trainset)
predictions <- predict(prune.tree, testset)


pred_result <- as.factor(ifelse(predictions[,1] > 0.2, 0, 1))
levels(pred_result) <- c('yes', 'no')

tb <- table(testset$churn, pred_result)
cm <- confusionMatrix(tb)
sens <- cm$byClass[1]
spec <- cm$byClass[2]

# DIY ROC Curve
res <- data.frame()
for (cost in seq(0,1,0.1)){
  pred_result <- as.factor(ifelse(predictions[,1] >= cost, 0, 1))
  levels(pred_result) <- c('yes', 'no')
  
  tb <- table(testset$churn, pred_result)
  cm <- confusionMatrix(tb)
  sens <- cm$byClass[1]
  spec <- cm$byClass[2]
  res <- rbind(res, data.frame('TPR' = sens, 'FPR' = 1 - spec)) 
}

res[is.na(res[,2]), 2] <- 0
res <- rbind(res, data.frame('TPR' = 1, 'FPR' = 1))

plot(TPR ~ FPR, data = res, type= 'l', col='red', xlim=c(0,1))
points(TPR ~ FPR, data = res, col='blue')


## Use ROCR
library(ROCR)
predictions  <- predict(churn.rp, testset, type="prob")
pred.to.roc  <- predictions[, 1]
pred.rocr    <- prediction(pred.to.roc, as.factor(testset$churn))
perf.rocr    <- performance(pred.rocr, measure ="auc", x.measure="cutoff")
perf.tpr.rocr<-performance(pred.rocr, "tpr","fpr")
plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))



## Clustering
data(iris)
data <- iris[,-5]
class<- iris[,5]

results <- kmeans(data,3)
results$cluster

par(mfrow=c(1,2))
plot(data$Petal.Length ~ data$Petal.Width, col=results$cluster)
plot(data$Petal.Length ~ data$Petal.Width, col=class)

## PCA Application
dataset <-read.csv('/tmp/eco_index.csv',head=TRUE, sep=',', row.names=1)
pc.cr   <- princomp(dataset, cor=TRUE)
plot(pc.cr)


screeplot(pc.cr, type="lines")
abline(h=1, lty=3, col="red")
biplot(pc.cr)
barplot(sort(pc.cr$score[,1], decreasing = TRUE), col=as.factor(rownames(pc.cr$scores)))
