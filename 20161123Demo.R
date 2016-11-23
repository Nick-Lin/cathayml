# 使用rpart 做iris 分類
data(iris)
View(iris)

library(rpart)
fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
fit
plot(fit, margin=0.1)
text(fit)
