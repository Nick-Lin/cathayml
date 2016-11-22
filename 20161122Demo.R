
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
