# SVM on iris datast using R

require(e1071)

# read the iris dara
data("iris")
head(iris)

#view structure of data
str(iris)

# Divide features and Outcome(Species) into separate datasets

require(dplyr)
train<-sample_frac(iris, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-iris[-sid,]


# build classifier on training set using radial kernel
svmModel_radial <- svm(Species ~ ., data = train, kernel = "radial")
summary(svmModel_radial)

pred_radial <- predict(svmModel_radial, test)

#view confusion matrix to check results
table(pred_radial, test$Species)


# Now build classifier using linear kernel
svmModel_linear <- svm(Species ~ ., data = train, kernel = "linear")
summary(svmModel_linear)

pred_linear <- predict(svmModel_linear, test)
table(pred_linear, test$Species)

# Thus, we can see that with a linear kernel we have only 2 mislassifications and 
#with the radial kenel , we have 5 misclassifications


# using the linear kernel, try different values for c and gamma

#cost = 1
#gamma = 0.5
svmModel_linear_1 <- svm(Species ~ ., data = train, kernel = "linear", cost = 1, gamma = 0.5)
summary(svmModel_linear_1)

pred_linear_1 <- predict(svmModel_linear_1, test)
table(pred_linear_1, test$Species)

# Misclassifications: 2

#cost = 0.1
#gamma = 0.5
svmModel_linear_2 <- svm(Species ~ ., data = train, kernel = "linear", cost = 0.1, gamma = 0.5)
summary(svmModel_linear_2)

pred_linear_2 <- predict(svmModel_linear_2, test)
table(pred_linear_2, test$Species)

# Misclassifications: 5


#cost = 100
#gamma = 0.5
svmModel_linear_3 <- svm(Species ~ ., data = train, kernel = "linear", cost = 100, gamma = 0.5)
summary(svmModel_linear_3)

pred_linear_3 <- predict(svmModel_linear_3, test)
table(pred_linear_3, test$Species)

# Misclassifications: 5


# Thus, a linear kernel with cost as 1 and gamma as 0.25 or 0.5 gives us minimum classification error
# Following that we have the radial classification 
