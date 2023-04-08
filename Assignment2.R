list.of.packages <- c("readr", "class","e1071","rpart","rpart.plot","caret","naivebayes","pROC","ROCR","dplyr","knitr","klaR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,,repos = "http://cran.us.r-project.org")
#if(length(new.packages)) remove.packages(new.packages)

library('readr')
library('class')
library('e1071')
library('rpart')
library('rpart.plot')
library('naivebayes')
library('caret')
library('pROC')
library('ROCR')
library('knitr')
library('dplyr')
library('klaR')

# Preparing Data
df <- read_csv('Dry_Bean_Dataset.csv')
set.seed(60)
rows <- sample(2,nrow(df),replace=TRUE, prob=c(0.7,0.3))
df$Class<-as.factor(df$Class)
train<-df[rows==1,] 
test<-df[rows==2,]
dim(train)
dim(test)

# ***********************************Decision Trees**********************************************
#Training
fit <- rpart(Class~., data = train, method = 'class') 
rpart.plot(fit, extra = 106,cex=0.75)
#predicting
predict_unseen <-predict(fit, test, type = 'class') 


# Metrics
table_mat <- table(test$Class, predict_unseen)
#Accuracy
accuracy_Test1 <- sum(diag(table_mat)) / sum(table_mat) 
print(paste('Accuracy for test', accuracy_Test1))
n = sum(table_mat) # number of instances
nc = nrow(table_mat) # number of classes
rowsums = apply(table_mat, 1, sum) # number of instances per class
colsums = apply(table_mat, 2, sum) # number of predictions per class
diag = diag(table_mat)  # number of correctly classified instances per class 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

print(" ************ Confusion Matrix ************")
print(table_mat)
print(" ************ Diag ************")
print(diag)
print(" ************ Precision/Recall/F1 ************")
print(data.frame(precision, recall, f1)) 
macroPrecision1 = mean(precision)
macroRecall1 = mean(recall)
macroF11 = mean(f1)

print(" ************ Macro Precision/Recall/F1 ************")
print(data.frame(macroPrecision1, macroRecall1, macroF11)) 
print(" ************ AUC ************")
roc.multi <- multiclass.roc(as.numeric(test$Class), as.numeric(predict_unseen))
print(auc(roc.multi))

confusionMatrix(predict_unseen,test$Class)
x=confusionMatrix(predict_unseen,test$Class)
mean(x$byClass[, "Sensitivity"])
mean(x$byClass[, "Specificity"])
Accuracy_Results <- tibble(Method = "Decision Tree", 
                           Accuracy = accuracy_Test1,
                           macroPrecision=macroPrecision1,
                           macroRecall=macroRecall1,
                           macroF1=macroF11,
                           Sensitivity=mean(x$byClass[, "Sensitivity"]),
                           Specificity=mean(x$byClass[, "Specificity"]))

Accuracy_Results %>% knitr::kable()

# ********************************Support Vector Machines*********************************************

classifier = svm(formula = Class ~ ., data = train,type = 'C-classification',kernel = 'linear')
y_pred = predict(classifier, newdata = test) 
summary(classifier)



#Metrics


table_mat <- table(test$Class, y_pred)
#Accuracy
accuracy_Test2 <- sum(diag(table_mat)) / sum(table_mat) 
print(paste('Accuracy for test', accuracy_Test2))
n = sum(table_mat) # number of instances
nc = nrow(table_mat) # number of classes
rowsums = apply(table_mat, 1, sum) # number of instances per class
colsums = apply(table_mat, 2, sum) # number of predictions per class
diag = diag(table_mat)  # number of correctly classified instances per class 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

print(" ************ Confusion Matrix ************")
print(table_mat)
print(" ************ Diag ************")
print(diag)
print(" ************ Precision/Recall/F1 ************")
print(data.frame(precision, recall, f1)) 
macroPrecision2 = mean(precision)
macroRecall2 = mean(recall)
macroF12= mean(f1)

print(" ************ Macro Precision/Recall/F1 ************")
print(data.frame(macroPrecision2, macroRecall2, macroF12)) 
print(" ************ AUC ************")
roc.multi <- multiclass.roc(as.numeric(test$Class), as.numeric(y_pred))
print(auc(roc.multi))

confusionMatrix(y_pred,test$Class)

x=confusionMatrix(y_pred,test$Class)
mean(x$byClass[, "Sensitivity"])
mean(x$byClass[, "Specificity"])

Accuracy_Results <- bind_rows(Accuracy_Results,
                              tibble(Method = "SVMs", 
                                     Accuracy = accuracy_Test2,macroPrecision=macroPrecision2,
                                     macroRecall=macroRecall2,
                                     macroF1=macroF12,
                                     Sensitivity=mean(x$byClass[, "Sensitivity"]),
                                     Specificity=mean(x$byClass[, "Specificity"])))
Accuracy_Results %>% knitr::kable()

# *************************************Naive Bayes***********************************************
model <- naive_bayes(Class ~ ., data = train, usekernel = T)
model
summary(model)
plot(model)
p1 <- predict(model, test) 



#Metrics
table_mat <- table(test$Class, p1)
#Accuracy
accuracy_Test3 <- sum(diag(table_mat)) / sum(table_mat) 
print(paste('Accuracy for test', accuracy_Test3))
n = sum(table_mat) # number of instances
nc = nrow(table_mat) # number of classes
rowsums = apply(table_mat, 1, sum) # number of instances per class
colsums = apply(table_mat, 2, sum) # number of predictions per class
diag = diag(table_mat)  # number of correctly classified instances per class 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

print(" ************ Confusion Matrix ************")
print(table_mat)
print(" ************ Diag ************")
print(diag)
print(" ************ Precision/Recall/F1 ************")
print(data.frame(precision, recall, f1)) 
macroPrecision3 = mean(precision)
macroRecall3 = mean(recall)
macroF13 = mean(f1)

print(" ************ Macro Precision/Recall/F1 ************")
print(data.frame(macroPrecision3, macroRecall3, macroF13)) 
print(" ************ AUC ************")
roc.multi <- multiclass.roc(as.numeric(test$Class), as.numeric(p1))
print(auc(roc.multi))






response <- as.factor(df$Class)
set.seed(12345)
train.idx <- sample(seq_len(nrow(df)), 0.8 * nrow(df))
df.train <- df[train.idx, ]
df.test <- df[-train.idx, ]
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab="Precision",
     xlab="Recall",
     bty='n')
colors <- c("red", "blue", "green","grey","yellow","black","pink")
aucs <- rep(NA, length(levels(response)))
# store AUCs
levels(response)
for (i in seq_along(levels(response))) {
  
  cur.class <- levels(response)[i]
  binary.labels <- as.factor(df.train$Class == cur.class)
  # binarize the classifier you are using (NB is arbitrary)
  model <- NaiveBayes(binary.labels ~ ., data = df.train[, -17])
  pred <- predict(model, df.test[,-17], type='raw')
  score <- pred$posterior[, 'TRUE'] # posterior for  positive class
  test.labels <- df.test$Class == cur.class
  pred <- prediction(score, test.labels)
  perf <- performance(pred, "prec", "rec")
  roc.x <- unlist(perf@x.values)
  roc.y <- unlist(perf@y.values)
  lines(roc.y ~ roc.x, col = colors[i], lwd = 2)
  # store AUC
  auc <- performance(pred, "auc")
  auc <- unlist(slot(auc, "y.values"))
  aucs[i] <- auc
}
lines(x=c(0,1), c(0,1))
legend("bottomright", levels(response), lty=1, 
       bty="n", col = colors)


confusionMatrix(p1,test$Class)
x=confusionMatrix(p1,test$Class)
mean(x$byClass[, "Sensitivity"])
mean(x$byClass[, "Specificity"])



Accuracy_Results <- bind_rows(Accuracy_Results,
                              tibble(Method = "Naive Bayes", 
                                     Accuracy = accuracy_Test3,macroPrecision=macroPrecision3,
                                     macroRecall=macroRecall3,
                                     macroF1=macroF13,
                                     Sensitivity=mean(x$byClass[, "Sensitivity"]),
                                     Specificity=mean(x$byClass[, "Specificity"])))
Accuracy_Results %>% knitr::kable()


# ******************************************KNN************************************************

medctrl <- trainControl(method="repeatedcv",number =10 ,repeats = 3)
knnFit <- train(Class ~ ., data = train, method = "knn", trControl = medctrl, preProcess = c("center","scale"), tuneLength = 20, na.action="na.omit")
print(knnFit)
pred <- predict(knnFit, newdata = test)
plot(knnFit)
summary(knnFit)

#Metrics
table_mat <- table(actualclass=test$Class, predictedclass= pred)

#Accuracy
accuracy_Test4 <- sum(diag(table_mat)) / sum(table_mat) 
print(paste('Accuracy for test', accuracy_Test4))
n = sum(table_mat) # number of instances
nc = nrow(table_mat) # number of classes
rowsums = apply(table_mat, 1, sum) # number of instances per class
colsums = apply(table_mat, 2, sum) # number of predictions per class
diag = diag(table_mat)  # number of correctly classified instances per class 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

print(" ************ Confusion Matrix ************")
print(table_mat)
print(" ************ Diag ************")
print(diag)
print(" ************ Precision/Recall/F1 ************")
print(data.frame(precision, recall, f1)) 
macroPrecision4 = mean(precision)
macroRecall4 = mean(recall)
macroF14 = mean(f1)

print(" ************ Macro Precision/Recall/F1 ************")
print(data.frame(macroPrecision4, macroRecall4, macroF14)) 
print(" ************ AUC ************")
roc.multi <- multiclass.roc(as.numeric(test$Class), as.numeric(pred))
print(auc(roc.multi))


x=confusionMatrix(pred,test$Class)

Accuracy_Results <- bind_rows(Accuracy_Results,
                              tibble(Method = "KNN", 
                                     Accuracy = accuracy_Test4,
                                     macroPrecision=macroPrecision4,
                                     macroRecall=macroRecall4,
                                     macroF1=macroF14,
                                     Sensitivity=mean(x$byClass[, "Sensitivity"]),
                                     Specificity=mean(x$byClass[, "Specificity"])))
Accuracy_Results %>% knitr::kable()