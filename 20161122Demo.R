
## Regression Analysis
X <- c(17, 21,35, 39, 50, 65)
Y <- c(132, 150, 160, 162, 149, 170)
plot(X, Y, xlim=c(0,70), ylim=c(0,200))

X_avg <- mean(X)
Y_avg <- mean(Y)

B1 <- (sum(X*Y)-6*X_avg*Y_avg)/(sum(X^2)-6*X_avg^2)
B1

B0 <- Y_avg- B1 * X_avg
B0

# y = 0.5529606 * x + 132.913
x <- 1:70
y <- 0.5529606 * x + 132.913
lines(x,y, col="red")

# use lm => linear model
X <- c(17,   21,  35,  39,  50,  65)
Y <- c(132, 150, 160, 162, 149, 170)
fit <- lm(Y ~ X)
fit
?lm


## 使用Quartet 資料集
library(car)
data(Quartet)
View(Quartet)
plot(y1 ~ x, data = Quartet)

## 使用lm 做一項式迴歸分析
fit <- lm(y1 ~ x, data = Quartet)
fit

## 將迴歸模型繪製在圖上
abline(fit, col="red")

## 產生預測結果
predict(fit, data.frame(x=c(16, 18)))
16 * 0.5001 + 3.0001
18 * 0.5001 + 3.0001


plot(y2 ~ x, data = Quartet)
fit1 <- lm(y2 ~ x, data = Quartet)
fit1
abline(fit1, col="red")


## 使用lm 做二項式迴歸分析
fit2 <- lm(y2 ~ poly(x,2), data = Quartet)
fit2
# Y = -3.712 * x ^ 2 + 5.244 * x + 7.501

Quartet$x
fit2$fit

## 使用lines 繪製模型線
lines(Quartet$x, fit2$fit, col="blue")
sort(Quartet$x)
fit2$fit[order(Quartet$x)]

plot(y2 ~ x, data = Quartet)
lines(sort(Quartet$x), fit2$fit[order(Quartet$x)], col="blue")
predict(fit2, data.frame(x=c(16, 18)))

## 可容錯的回歸(rlm)
plot(y3 ~ x, data = Quartet)
fit3 <- lm(y3 ~ x, data = Quartet)
abline(fit3, col="red")

library(MASS)
?rlm
fit4 <- rlm(y3 ~ x, data = Quartet)
abline(fit4, col="blue")

## 分析台北市大安區 591租屋網的租金資訊
house <- read.csv('/tmp/591.csv', header = TRUE)
plot(Price ~ Area, data= house)
fit <- lm(Price ~ Area, data= house)
fit
abline(fit, col="red")
predict(fit, data.frame(Area = c(20)))

## 線性模型評估
data(Quartet)
plot(y2 ~ x, data = Quartet)
fit1 <- lm(y2 ~ x, data = Quartet)
abline(fit1, col="red")
summary(fit1)

fit2 <- lm(y2 ~ poly(x,2), data = Quartet)
summary(fit2)
lines(sort(Quartet$x), fit2$fit[order(Quartet$x)], col="blue")

plot(y1 ~ x, data = Quartet)
fit <- lm(y1 ~ x, data = Quartet)
abline(fit, col="red")

## 線性模型評估 (lm)
plot(y3 ~ x, data = Quartet)
lmfit <- lm(y3~x, data = Quartet)
abline(lmfit, col="red")
predicted <- predict(lmfit, newdata=Quartet[c("x")])
predicted

actual <- Quartet$y3

rmse   <- (mean((predicted -actual)^2))^0.5
rmse

mu      <- mean(actual)
rse     <- mean((predicted -actual)^2)/mean((mu - actual)^2)
rsquare <- 1 - rse
rsquare


## 線性模型評估 (rlm)
plot(y3 ~ x, data = Quartet)
lmfit <- rlm(y3~x, data = Quartet)
abline(lmfit, col="red")
predicted <- predict(lmfit, newdata=Quartet[c("x")])
predicted

actual <- Quartet$y3

rmse   <- (mean((predicted -actual)^2))^0.5
rmse

mu      <- mean(actual)
rse     <- mean((predicted -actual)^2)/mean((mu - actual)^2)
rsquare <-  1 - rse
rsquare


## 多元迴歸分析(Multiple Regression)
house_prices <- read.csv(file="/tmp/house-prices.csv")
names(house_prices)
str(house_prices)
lm.1 <-lm(Price ~ SqFt, data=house_prices)
summary(lm.1)


house_prices$brick_d<-ifelse(house_prices$Brick=="Yes",1,0)
house_prices$east   <-ifelse(house_prices$Neighborhood=="East",1,0)
house_prices$north  <-ifelse(house_prices$Neighborhood=="North",1,0)


set.seed(110)
sub<-sample(nrow(house_prices), floor(nrow(house_prices)*0.6))
training_data   <- house_prices[sub,]
validation_data <- house_prices[-sub,]


lm.fit1 <-lm(Price ~ SqFt+Bathrooms+Bedrooms+Offers+north+east+brick_d,data=training_data)
summary(lm.fit1)
step(lm.fit1)
vif(lm.fit1)

training_data$predict.price  <- predict(lm.fit1)
training_data$error          <- residuals(lm.fit1)

validation_data$predict.price <- predict(lm.fit1, newdata = validation_data)
validation_data$error         <- validation_data$predict.price - validation_data$Price 

hist(training_data$error)
hist(validation_data$error)

a<-cor(training_data$Price,training_data$predict.price)
b<-cor(validation_data$Price,validation_data$predict.price)
a * a
b * b

## 使用step 與 stepAIC 挑變數
step(lm.fit1)

library(MASS)
?stepAIC
stepAIC(lm.fit1)

## 使用step 與 stepAIC 挑591房屋網變數
house <- read.csv('/tmp/591.csv', header = TRUE)
names(house)
fit <- lm(Price ~ Area + Floor + TotalFloor + Bedroom + Living.Room + Bathroom, data= house)
summary(fit)
lm.fit.step <- step(fit)
summary(lm.fit.step)
