library('readr')
library('class')
library('e1071')
library('rpart')
library('rpart.plot')
library('naivebayes')
library('caret')
library('pROC')
#library(tidyverse)
library(caret)
#library(xgboost)
library('ROCR')
library(randomForest)
library(datasets)
library(caret)
df <- read_csv('Dry_Bean_Dataset.csv')
str(df)
set.seed(60)
rows <- sample(2,nrow(df),replace=TRUE, prob=c(0.75,0.25))
df$Class<-as.factor(df$Class)
train<-df[rows==1,] 
test<-df[rows==2,]
dim(train)
dim(test)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(df$Class, times = 1, p = 0.34, list = FALSE)
test_x <- df[test_index,-17]
test_y <- df$Class[test_index]
train_x <- df[-test_index,-17]
train_y <- df$Class[-test_index]

# **************************************************************BAGGING*************************
print("**************************************************************BAGGING*************************")
print(" ** NOTE: THIS WILL TAKE 1-2 Minutes on a typical system **")
set.seed(9874, sample.kind="Rounding")
trCtrl <- trainControl(method = "cv", number = 5)
cr.fit <- train(train_x, train_y,
                method  = "treebag",
                trControl = trCtrl,
                metric = "Accuracy")
cr.fit
crtb_acc <- cr.fit$results[,2]
p2 <- predict(cr.fit, test_x)

confusionMatrix(p2, test_y)


# *******************************************RANDOM FOREST ******************************
print("*******************************************RANDOM FOREST ******************************")

train$Class<-as.factor(train$Class)
test$Class<-as.factor(test$Class)
rf <- randomForest(Class ~.,ntree=400, data=train,weights=NULL,
                   replace=TRUE)
print(rf)


plot(rf)
varImpPlot(rf,sort = T,n.var = 10,main = "Top 10 - Variable Importance")
importance(rf)

p2 <- predict(rf, test)
confusionMatrix(p2, test$Class)

# ************************************** K FOLD CROSS VALIDATION ************************
print("************************************** K FOLD CROSS VALIDATION ************************")
classifier = svm(formula = Class ~ ., data = train,type = 'C-classification',kernel = 'linear')
y_pred = predict(classifier, newdata = test) 


# in creating the folds we specify the target feature (dependent variable) and # of folds
folds = createFolds(train$Class, k = 10)
# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = train[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = train[x, ] # here we describe the test fold individually
  new_test=test    
  # now apply (train) the classifer on the training_fold
  
  classifier = svm(formula = Class ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'linear')
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier,
                   newdata = test_fold)
  pred = predict(classifier,
                 newdata=new_test )
  
  
  cm=table(test_fold$Class, y_pred)
  accuracy_Test1 <- sum(diag(cm)) / sum(cm) 
  cm1=table(new_test$Class, pred)
  accuracy_Test <- sum(diag(cm1)) / sum(cm1) 
  
  
  return(accuracy_Test1)
})


p1 <- predict(classifier,test)
confusionMatrix(p1, test$Class)


# ************************************** REPEATED CV ****************************

print("************************************** REPEATED CV ****************************")
set.seed(56543, sample.kind="Rounding")
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 
grid <- expand.grid(C = seq(0.85, .95, 0.01))
print(" ** NOTE: THIS WILL TAKE 3-5 Minutes on a typical system **")
svm_Linear_Grid <- train(train_x, train_y, 
                         method = "svmLinear",
                         trControl=trctrl,
                         tuneGrid = grid,
                         tuneLength = 10)

# Print the best tuning parameter C that
# maximizes model accuracy

svm_Linear_Grid$bestTune

plot(svm_Linear_Grid)

# reported Accuracy from the cross validation in the train set only
svm_LG_Acc <- svm_Linear_Grid$results[3,2]

# reported Accuracy from the cross validation in the train set only

print(svm_LG_Acc)

print(svm_Linear_Grid)


