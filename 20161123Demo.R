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
