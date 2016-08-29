# Classification on stock market data (whether it goes up or down)

library(ISLR)
data("Smarket")
summary(Smarket)

test_id <- which(1:length(Smarket[,2])%%4 == 0)

Smarket_train <- Smarket[-test_id,]
Smarket_test <- Smarket[test_id,]


#find corelations between Direction and predictors
pairs(Smarket, col = Smarket$Direction)
logit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume + Today, 
             data = Smarket_train, family = 'binomial')


summary(logit)
logit

pred_logit <- predict.glm(logit, newdata = Smarket_test, type = 'response')


table(pred_logit,Smarket_test$Direction)
#https://dzone.com/refcardz/machine-learning-predictive
#Multiclass Logistic Regression
data("iris")
str(iris)

test_ids <- which(1:length(iris[,1])%%5 == 0)

iris_train <- iris[-test_ids,]
iris_test <- iris[test_ids,]

install.packages("nnet")
library(nnet)
clf <- multinom(formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                data = iris_train)

pred <- predict(clf, newdata = iris_test, type = 'class')
pred

str(pred)

table(pred, iris_test$Spe)

#pred_df <- as.data.frame(cbind(iris_test$Species, pred))

#error <- lapply(pred_df, function(i) pred_df[i,1] != pred_df[i,2])



#SVM
library(e1071)

tune <- tune.svm(Species ~ . , data = iris_train, gamma = 10^(-10:-1), cost = 10^(1:10))
summary(tune)

svm_clf <- svm(Species ~ ., data = iris_train, gamma = 0.00005, method = 'C-Classification',
               cost = 10000000, kernel = 'radial', probability = T)

pred_svm <- predict(svm_clf, newdata = iris_test, probability = T)

table(pred_svm,iris_test$Species)


