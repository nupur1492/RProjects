# Following code from the following forum: 
# https://www.kaggle.com/otiksaw/digit-recognizer/minst-using-randomforest-svm-and-neuralnet/code



# Read train and test data
df_train <- read.csv("train.csv")
df_test <- read.csv("test.csv")

# Check if the data is uniformly distributed over all labels
label.freq = table(df_train$label)
#barplot(label.freq)

# Check if NAs exist:
which(is.na(df_train))
# No NAs present

# convert lable to a categorical variable
df_train$label <- as.factor(df_train$label)

# separate pixels from the training and testing sets
df_train_pixel <- df_train[,2:ncol(df_train)]
df_test_pixel <- df_test[,2:ncol(df_test)]


## Excluding highly correlated variables
# Obtaining Correlation matrix
MatCor <- cor(df_train[sapply(df_train,is.numeric)])

# The standard deviation is zero, hence correlation matrix cannot be used

# Use nearZeroVar to preprocess the pixels
library(caret)

#df_zeroVar <- as.data.frame(colnames(df_train_pixel[nearZeroVar(df_train_pixel)]))

train_nzv <- nearZeroVar(df_train_pixel)
test_nzv <-  nearZeroVar(df_test_pixel)
# nearZeroVar selects 532 out of 784 factors/pixels

# create a new dataframe with these 532 factors. 
# This dataframe will be used for further preprocessing

df_train_postnzv <- df_train_pixel[,-train_nzv]
df_test_postnzv <- df_train_pixel[,-train_nzv]

# Preprocess using PCA
train_preProcValues <- preProcess(df_train_postnzv, method=c("pca"))
test_preProcValues <- preProcess(df_test_postnzv, method = c("pca"))

x_trainTransformed <- predict(train_preProcValues, df_train_postnzv)
x_testTransformed <- predict(test_preProcValues, df_test_postnzv)


dim(x_trainTransformed)
dim(x_testTransformed)



### Building Models 

# 1. Multiclass logistic regression
df_train_final <- x_trainTransformed
df_train_final$label <- df_train$label

dim(df_train_final)

LR_Model <- glm(label~., 
                family=binomial(link='logit'), data=df_train_final)




LR_Fitted <- predict(LR_Model, newdata= x_testTransformed, type = "response")

LR_Fitted_Binary



LR_confusionMatrix <- confusionMatrix(LR_Fitted, df_train_final$label)
LR_confusionMatrix

#Error in confusionMatrix.default(LR_Fitted, df_train_final$label) : 
#the data cannot have more levels than the reference

table(factor(LR_Fitted, levels=min(x_testTransformed):max(x_testTransformed)),
      factor(x_testTransformed, levels=min(x_testTransformed):max(x_testTransformed)))


LR_accuracy <- as.numeric(LR_confusionMatrix$overall["Accuracy"])



# Random forests:
library(randomForest)

samplerows <- sample(1:nrow(df_train_pixel), nrow(df_train)*0.6, replace=FALSE)
df_train_rf <- x_trainTransformed[samplerows,]
df_test_rf <- x_trainTransformed[-samplerows,]

train_labels <- as.factor(df_train[samplerows,]$label)
test_labels<- as.factor(df_train[-samplerows]$labels)


RF_Model <- randomForest(df_train_rf, train_labels, ntree = 10)

predict_labels <- predict(RF_Model, df_test_rf)

table(predict_labels)



# SVM
library(e1071)
SVM_Model <- svm(label ~., data = df_train_final)