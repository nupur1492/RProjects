#Regression on Boston housing dataset

library(MASS) 
library(ISLR)

names(Boston)

df_boston <- Boston
str(df_boston)

plot(medv~lstat, Boston)

fit_reg <- lm(medv ~ lstat, Boston)
fit_reg
summary(fit_reg)

#fitting a line
abline(fit_reg, col = "red")

#confidence intervals
confint(fit_reg)

#predict
predict(fit_reg, data.frame(lstat = c(15,30,45)), interval = "confidence")

# multiple linear Regression
fit1_mlr <- lm(medv ~ lstat + age, Boston)
summary(fit1_mlr)

# the one with all predictors
fit2_mlr <- lm(medv ~ ., Boston)
summary(fit2_mlr)

#remove less significant predictors
fit3_mlr <- update(fit2_mlr,~. -age-indus)
summary(fit3_mlr)

plot(fit3_mlr)

#Non linear terms and interactions:
fit4_mlr <- lm(medv ~ lstat*age, Boston)
summary(fit4_mlr)

fit5_mlr <- lm(medv ~ lstat + I(lstat^2), Boston)
summary(fit5_mlr)
# both linear and quadratic coefficients are significant

plot(medv ~ lstat, Boston)
points(Boston$lstat, fitted(fit5_mlr),pch = 20, col = "red")

#Qualitative predictors
data("Carseats")
summary(Carseats)

qfit1 <- lm(Sales ~ .,Carseats)
summary(qfit1)

#Function for plotting different predictors
reg_plot <- function(x,y,...) {
  ffit <- lm(y~x)
  plot(x,y,...)
  abline(ffit, col = "red")
}

reg_plot(Carseats$Advertising, Carseats$Sales, 
         xlab = "Advertising", ylab = "Sales", 
         col = "green", pch=20)
