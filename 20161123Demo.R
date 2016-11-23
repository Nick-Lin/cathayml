# 使用rpart 做iris 分類
data(iris)
View(iris)

library(rpart)
fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
fit
plot(fit, margin=0.2)
text(fit)


plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)
abline(v = 2.45, col="blue", lwd = 3)
abline(h = 1.75, col="orange", lwd= 3)

## 驗證預測結果
head(predict(fit, newdata = iris))
predicted <- predict(fit, newdata = iris, type= "class")
table(predicted, iris$Species)


# 使用ctree 做iris 分類
library(party)
fit <- ctree(Species ~ ., data = iris)
plot(fit)

plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)
plot(fit)
abline(v = 1.9, col="orange", lwd= 3)
abline(h = 1.7, col="red", lwd= 3)
abline(v = 4.8, col="blue", lwd = 3)



predicted <- predict(fit, newdata = iris, type= "response")
table(predicted, iris$Species)


## Logistic Regression
data(iris)
iris         <- iris[(iris$Species!="setosa"),]
iris$Species <- factor(iris$Species)
fit          <- glm(Species~.,iris,family=binomial)
summary(fit)

predicted <- predict(fit, iris[,1:4], type= "response")
table(ifelse(predicted > 0.5, 'virginica', 'versicolor'), iris[,5])

## 比較決策樹的分類結果
fit2        <- ctree(Species~.,data=iris)
predicted2 <- predict(fit2, iris[,1:4], type= "response")
table(predicted2, iris[,5])



library(e1071)
data(iris)
fit <-svm(Species~.,iris)
summary(fit)
table(predict(fit, iris[,-5]), iris[,5])


## SVM with lower cost
iris.subset = subset(iris, select=c("Sepal.Length", "Sepal.Width", "Species"), Species %in% c("setosa","virginica"))
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
svm.model = svm(Species ~ ., data=iris.subset, kernel='linear', cost=1, scale=FALSE)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)

## SVM with higher cost
svm.model <- svm(Species ~ ., data=iris.subset, type='C-classification', kernel='linear', cost=10000, scale=FALSE)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)


## http://scikit-learn.org/stable/_images/sphx_glr_plot_iris_0012.png


## 使用caret 呼叫不同的機器學習函式
library(caret)

rpartfit <- train(Species ~.  , data = iris, method = "rpart")
rffit    <- train(Species ~.  , data = iris, method = "rf")
svmfit   <- train(Species ~.  , data = iris, method = "svmLinear")
names(getModelInfo())
