library(readxl)
library(caret)

sample_data <- read_excel("~/Desktop/Business Data Science/Case study/data.xlsx",sheet=5)

full_data <- read_excel("~/Desktop/Business Data Science/Case study/data.xlsx",sheet=4)

sample_data$Manipulator <- as.factor(sample_data$Manipulator)

full_data$Manipulater <- as.factor(full_data$Manipulater)


# Q2

set.seed(1000)


#SMOTE method - Complete dataset
library(unbalanced)

n1<-ncol(full_data)
output1<- full_data$`C-MANIPULATOR`
output1<-as.factor(output1)
input1<- full_data [ ,-n1]
# View(input1)

data1<-ubBalance(X= input1, Y=output1, type="ubSMOTE",percOver=700, percUnder=350)
# View(data1)

balanced_full_data<-cbind(data1$X,data1$Y)
# View(balancedData_full)

names(balanced_full_data)[11] <- "C_Manipulator"
table(balanced_full_data$C_Manipulator)

#Renaming the variable in sample dataset
colnames(sample_data)[ncol(sample_data)] <- "C_Manipulator"

sample_data <- sample_data[,-1]
balanced_full_data <- balanced_full_data[,-1]

# Q3, Q4, Q5

# Run the logistic regression

# Sample data unbalanced
set.seed(1000)
fit <- glm(C_Manipulator ~ . -Manipulator , data = sample_data, family = "binomial")
summary(fit)

train_predict <- predict(fit, sample_data, type = "response")
train_predict <- data.frame(train_predict)
colnames(train_predict)[1] <- "V1"

table(train_predict$V1, sample_data$C_Manipulator)

df <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())
t_s <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(train_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, sample_data$C_Manipulator))
  
  t <- table(conf_data$V1,conf_data$sample_data.C_Manipulator)
  t1 <- data.frame(table(conf_data$V1, conf_data$sample_data.C_Manipulator))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$sample_data.C_Manipulator,positive='1')      
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}


summary(t_s)

select_df <- subset(t_s, YI==max(YI))
colMeans(select_df)
t_s <- rbind(t_s,df)
  





# Q6
# Creating the M-score

manipulated_data <- sample_data
manipulated_data$SGAI <- 0
manipulated_data$LEVI <- 0

# M_score <- data.frame(predict(fit, manipulated_data, type = "response"))
M_score <- data.frame(predict(fit, manipulated_data))
M_threshold <- log(result_df$Threshold/(1-result_df$Threshold))



# Q7

#Creating CART tree for sample data

set.seed(1000)
library(rpart)
library(rpart.plot)
sample_tree <- rpart(C_Manipulator ~ .- Manipulator, data = sample_data, method="class", parms = list(split="gini"))
summary(sample_tree)
printcp(sample_tree)

#plotting
rpart.plot(sample_tree)

pred_train <- predict(sample_tree,sample_data,type = "class")
conf_mat <- table(pred_train,sample_data$C_Manipulator)

ss <- data.frame(pred_train, sample_data$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall ["Accuracy"]
vv$byClass ["Sensitivity"]



# Q8


# Full data balanced
#Stratified random sampling
set.seed(1000)
train.index <- createDataPartition(balanced_full_data$C_Manipulator, p = .6, list = FALSE)
balanced_full_data_train <- balanced_full_data[ train.index,]
balanced_full_data_test  <- balanced_full_data[-train.index,]



set.seed(1000)
fit <- glm(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, family = "binomial")
summary(fit)

test_predict <- predict.glm(fit, balanced_full_data_test, type = "response")
test_predict <- data.frame(test_predict)
colnames(test_predict)[1] <- "V1"

table(test_predict$V1, balanced_full_data_test$C_Manipulator)

df <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())
t_s <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(test_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, balanced_full_data_test$C_Manipulator))
  
  t <- table(conf_data$V1,conf_data$balanced_full_data_test.C_Manipulator)
  t1 <- data.frame(table(conf_data$V1, conf_data$balanced_full_data_test.C_Manipulator))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$balanced_full_data_test.C_Manipulator,positive = '1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}


summary(t_s)

select_df <- subset(t_s, YI==max(YI))
colMeans(select_df)
t_s <- rbind(t_s,df)

 


# Q9

## Creating CART tree for full balanced data

set.seed(1000)
library(rpart)
library(rpart.plot)
full_balanced_tree <- rpart(C_Manipulator ~ .- Manipulater, data = balanced_full_data_train, method="class", parms = list(split="gini"))
summary(full_balanced_tree)
printcp(full_balanced_tree)

#plotting
rpart.plot(full_balanced_tree)

pred_test <- predict(full_balanced_tree, balanced_full_data_test, type = "class")
#conf_mat <- table(pred_test,balanced_full_data_test$C_Manipulator)

ss <- data.frame(pred_test, balanced_full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall ["Accuracy"]
vv$byClass ["Sensitivity"]



# Creating Random forest model for full balanced data


library(caTools)
library(randomForest)
library(ROCR)


# Full data balanced
set.seed(1000)
mtry_rf=if (!is.null(balanced_full_data_train$C_Manipulator) && !is.factor(balanced_full_data_train$C_Manipulator)) max(floor(ncol(balanced_full_data_train)/3), 1) else floor(sqrt(ncol(balanced_full_data_train))) 
manipulation_rf = randomForest(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, ntree = 100, mtry = mtry_rf, proximity =TRUE, replace = TRUE, sampsize = nrow(balanced_full_data_train), importance = TRUE )


### results- different functions to view results
print(manipulation_rf)
attributes(manipulation_rf)

### To access the error rate 
plot(manipulation_rf)
manipulation_rf$err.rate
rndF1.legend <- if (is.null(manipulation_rf$balanced_full_data_test$err.rate)) {colnames(manipulation_rf$err.rate)} else {colnames(manipulation_rf$balanced_full_data_test$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

### Variable importance
importance(manipulation_rf)
dim(importance(manipulation_rf))


# Plot variable importance
varImpPlot(manipulation_rf)

### Get accuracy of prediction
# Predicting on training data
table(predict(manipulation_rf), balanced_full_data_train$C_Manipulator) #balanced_full_data_train

# Predicting on test data
pred_test <-  predict(manipulation_rf, balanced_full_data_test)
#conf_mat <- table(pred_test, balanced_full_data_test$C_Manipulator)

ss <- data.frame(pred_test, balanced_full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall ["Accuracy"]
vv$byClass ["Sensitivity"]



# Boosting using adaboost


library(adabag)

set.seed(1000)
adaboost <- boosting(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, mfinal = 40, control = rpart.control(maxdepth = 2))


boost_pred <- predict(adaboost, data.frame(balanced_full_data_test))

ss <- confusionMatrix(boost_pred$confusion,positive = '1')
ss$overall["Accuracy"]
ss$byClass ["Sensitivity"]


###  Boosting using mboost
library(mboost)

set.seed(1000)

boost_temp <- balanced_full_data_train[-9]


boost <- mboost(C_Manipulator ~ .  , data = boost_temp
                       ,control = boost_control(mstop = 300, nu = 0.5,
                                      risk = c("inbag", "oobag", "none"), stopintern = FALSE,
                                      center = TRUE, trace = FALSE)
                       , family= Binomial(type = c("adaboost", "glm"),
                                          link = c("logit")))
                      
                       
    
boost_pred <- predict(boost, balanced_full_data_test, type = "class")
#conf_mat <- table(boost_pred, balanced_full_data_test$C_Manipulator)

ss <- data.frame(boost_pred, balanced_full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall ["Accuracy"]
vv$byClass ["Sensitivity"]

#############################################################################################################
# Approach 2

library(readxl)
library(caret)

full_data <- read_excel("~/Desktop/Business Data Science/Case study/data.xlsx",sheet=4)


full_data$Manipulater <- as.factor(full_data$Manipulater)

names(full_data)[11] <- "C_Manipulator"


# Q2

set.seed(1000)
train.index <- createDataPartition(full_data$C_Manipulator, p = .6, list = FALSE)
full_data_train <- full_data[ train.index,]
full_data_test  <- full_data[-train.index,]

table(full_data_train$C_Manipulator)

#SMOTE method - training dataset
library(unbalanced)

n1<-ncol(full_data_train)
output1<- full_data_train$C_Manipulator
output1<-as.factor(output1)
input1<- full_data_train [ ,-n1]
# View(input1)

data1<-ubBalance(X= input1, Y=output1, type="ubSMOTE",percOver=700, percUnder=350)
# View(data1)

balanced_full_data_train<-cbind(data1$X,data1$Y)
# View(balancedData_full)

names(balanced_full_data_train)[11] <- "C_Manipulator"
names(full_data_test)[11] <- "C_Manipulator"
balanced_full_data_train <- balanced_full_data_train[,-1]
full_data_test <- full_data_test[,-1]

#Q8
 set.seed(1000)
#Logistic Regression
fit <- glm(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, family = "binomial")
summary(fit)

test_predict <- predict.glm(fit, full_data_test, type = "response")
test_predict <- data.frame(test_predict)
colnames(test_predict)[1] <- "V1"

table(test_predict$V1, full_data_test$C_Manipulator)

df <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())
t_s <- data.frame(Threshold=double(),Accuracy=double(),Sensitivity=double(),Fpr=double(),YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(test_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, full_data_test$C_Manipulator))
  
  t <- table(conf_data$V1,conf_data$full_data_test.C_Manipulator)
  t1 <- data.frame(table(conf_data$V1, conf_data$full_data_test.C_Manipulator))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$full_data_test.C_Manipulator,positive = '1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}


summary(t_s)

select_df <- subset(t_s, YI==max(YI))
colMeans(select_df)
t_s <- rbind(t_s,df)




# Q9

## Creating CART tree for training full balanced data

set.seed(1000)
library(rpart)
library(rpart.plot)
full_balanced_tree <- rpart(C_Manipulator ~ .- Manipulater, data = balanced_full_data_train, method="class", parms = list(split="gini"))
summary(full_balanced_tree)
printcp(full_balanced_tree)

#plotting
rpart.plot(full_balanced_tree)

pred_test <- predict(full_balanced_tree,full_data_test, type = "class")
#conf_mat <- table(pred_test,full_data_test$C_Manipulator)

ss <- data.frame(pred_test,full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall["Accuracy"]
vv$byClass ["Sensitivity"]



# Creating Random forest model for full balanced data


library(caTools)
library(randomForest)
library(ROCR)



set.seed(1000)
mtry_rf=if (!is.null(balanced_full_data_train$C_Manipulator) && !is.factor(balanced_full_data_train$C_Manipulator)) max(floor(ncol(balanced_full_data_train)/3), 1) else floor(sqrt(ncol(balanced_full_data_train))) 
manipulation_rf = randomForest(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, ntree = 100, mtry = mtry_rf, proximity =TRUE, replace = TRUE, sampsize = nrow(balanced_full_data_train), importance = TRUE )


### results- different functions to view results
print(manipulation_rf)
attributes(manipulation_rf)

### To access the error rate 
plot(manipulation_rf)
manipulation_rf$err.rate
rndF1.legend <- if (is.null(manipulation_rf$full_data_test$err.rate)) {colnames(manipulation_rf$err.rate)} else {colnames(manipulation_rf$full_data_test$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

### Variable importance
importance(manipulation_rf)
dim(importance(manipulation_rf))


# Plot variable importance
varImpPlot(manipulation_rf)

### Get accuracy of prediction
# Predicting on training data
table(predict(manipulation_rf), balanced_full_data_train$C_Manipulator) #balanced_full_data_train

# Predicting on test data
pred_test <-  predict(manipulation_rf,full_data_test)
conf_mat <- table(pred_test,full_data_test$C_Manipulator)

ss <- data.frame(pred_test,full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall["Accuracy"]
vv$byClass ["Sensitivity"]



# Boosting using adaboost


library(adabag)

set.seed(1000)
adaboost <- boosting(C_Manipulator ~ . -Manipulater , data = balanced_full_data_train, mfinal = 40, control = rpart.control(maxdepth = 2))


boost_pred <- predict(adaboost,data.frame(full_data_test))

ss <- confusionMatrix(boost_pred$confusion,positive = '1')
ss$overall["Accuracy"]
ss$byClass ["Sensitivity"]


###  Boosting using mboost
library(mboost)

set.seed(1000)

boost_temp <- balanced_full_data_train[-9]


boost <- mboost(C_Manipulator ~ .  , data = boost_temp
                ,control = boost_control(mstop = 300, nu = 0.5,
                                         risk = c("inbag", "oobag", "none"), stopintern = FALSE,
                                         center = TRUE, trace = FALSE)
                , family= Binomial(type = c("adaboost", "glm"),
                                   link = c("logit")))



boost_pred <- predict(boost, full_data_test, type = "class")
#conf_mat <- table(boost_pred,full_data_test$C_Manipulator)

ss <- data.frame(boost_pred,full_data_test$C_Manipulator)
vv <- confusionMatrix(ss[,1],ss[,2],positive = '1')

vv$overall["Accuracy"]
vv$byClass ["Sensitivity"]





