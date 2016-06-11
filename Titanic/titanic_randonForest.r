
#read train data
df_titanic_train <- read.csv("train.csv")
str(df_titanic_train)

summary(df_titanic_train$Age)
#age has 177 NAs. Must be substituted apporpriately

#predict age based on Pclass, sex, siblings, parents, fare and cabin
#use method anova
install.packages('rpart')
library(rpart)

age_pred <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Cabin,
                 data = df_titanic_train[!is.na(df_titanic_train$Age),],
                 method = "anova"
                 )
#replace NAs using age_pred model
df_titanic_train$Age[is.na(df_titanic_train$Age)] <- predict(age_pred,
                                                              df_titanic_train[is.na(df_titanic_train$Age),])

summary(df_titanic_train$Embarked)

# check which rows have a blank value in Embarked 
which(df_titanic_train$Embarked == '')
#62 830

# Replace blank values of Embarked column with S(Southampton), and convert to categorical
df_titanic_train$Embarked[c(62,830)] <- 'S'
df_titanic_train$Embarked <- factor(df_titanic_train$Embarked)

summary(df_titanic_train$Fare)

install.packages('randomForest')
library(randomForest)

set.seed(9999)

df_titanic_train$Survived <- as.factor(df_titanic_train$Survived)
df_titanic_train$Pclass <- as.factor(df_titanic_train$Pclass)

# initialize model
fit <- randomForest(Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked,
                    data = df_titanic_train,
                    ntree = 5000,
                    importance = TRUE)

#test model
df_titanic_test <- read.csv("test.csv")
str(df_titanic_test)
summary(df_titanic_test$Age)

age_pred_test <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Cabin,
                  data = df_titanic_test[!is.na(df_titanic_test$Age),],
                  method = "anova"
)


#replace NAs using age_pred_test model
df_titanic_test$Age[is.na(df_titanic_test$Age)] <- predict(age_pred_test,
                                      df_titanic_test[is.na(df_titanic_test$Age),])

#check Embarked and Fare for NAs
summary(df_titanic_test$Embarked)
summary(df_titanic_test$Fare)

#find row which is NA
which(is.na(df_titanic_test$Fare))

#replace NA with median value of fare
df_titanic_test$Fare[153] <- median(df_titanic_test$Fare, na.rm = TRUE)

# change type of Pclass, to match it with type in 
df_titanic_test$Pclass <- as.numeric(df_titanic_test$Pclass)
Prediction <- predict(fit, df_titanic_test)

df_predictions <- as.data.frame(Prediction)
df_predictions$PassengerId <- df_titanic_test$PassengerId
df_predictions <- df_predictions[,c(2,1)]

colnames(df_predictions) <- c('PassengerId','Survived')

#write.csv(df_predictions, "Titanic_RF.csv",row.names = FALSE)

#improve model
install.packages("party")
library(party)

set.seed(9999)
fit_better <- cforest(Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked,
                      data = df_titanic_train,
                      controls = cforest_unbiased(ntree = 2000, mtry=3))

Prediction_new <- predict(fit_better, df_titanic_test, OOB = TRUE, type = "response")

df_predictions <- as.data.frame(Prediction_new)
df_predictions$PassengerId <- df_titanic_test$PassengerId
df_predictions <- df_predictions[,c(2,1)]

colnames(df_predictions) <- c('PassengerId','Survived')

write.csv(df_predictions, "Titanic_RF.csv",row.names = FALSE)