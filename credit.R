#check the working directory
getwd()

library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(readr)
library(caret)

#

df <- read.csv("creditcard.csv")
str(df)
summary(df)
#
#There are no Null values in the data set.
#   but the Vairables are of different ranges.
#To perform Random forest this is fine
#   but to perform regression we have normalize the dataset
#
#____________________________________________#

table(df$Class)

# Base line accuracy

base_line_accuracy= 284315/(284315+492)
base_line_accuracy

df$Time =NULL
#The base_line_accuracy itself is 99.9%
# the dataset is highly imbalanced.
# So inorder to form a model, we need to perform Under_sampling or over_sampling

#First we use the data set as it is and predit the accuracy of model
set.seed(125)
split = sample.split(df$Class, SplitRatio = 0.7)
df_train = subset(df, split==TRUE)
df_test = subset(df, split==FALSE)
#

table(df_train$Class)
table(df_test$Class)

#Logistic regression

model1 <- glm(Class ~ ., data = df_train, family = binomial)
summary(model1)

#The AIC for the model is very high, so not reliable

model1_train_pred = predict(model1, newdata = df_train)
table(df_train$Class, model1_train_pred > 0.5)
model1_train_accuray = (198995 + 199)/(198995+25+145+199)
model1_train_accuray
#
model1_test_pred = predict(model1, newdata = df_test)
table(df_test$Class, model1_test_pred>0.4)
model1_test_accuracy = (85284+89)/(85284+11+59+89)
model1_test_accuracy

#Accuracy seems High, lets check precision and recall

model1_precision = 89/(89+11)
model1_precision
model1_recall = 89/(89+59)
model1_recall
#
model1_F_measure = 2*model1_precision*model1_recall/(model1_precision+model1_recall)
model1_F_measure

# precision and recall are poor for the model1

#_________________________________________________#
#
#Now normalize the data set and perform regression analysis
#

dat = df

str(dat)
summary(dat)
table(dat$Class)
#
#___Under sampling_________#


df$Class = as.factor(df$Class)
data.class.0 <- subset(df, df$Class == 0)
data.class.1 <- subset(df, df$Class == 1)
nrow(data.class.0)
nrow(data.class.1)
data.class.0 <- data.class.0[1:10000, ]
nrow(data.class.0)
data <- rbind(data.class.0, data.class.1)
nrow(data)

set.seed(1)
split <- sample.split(data$Class, SplitRatio = 0.7)
train <- subset(data, split == T)
cv <- subset(data, split == F)

table(cv$Class)

model2 <- glm(Class ~ ., data = train, family = "binomial", control = list(maxit = 50))
model2_predict <- predict(glm.model, cv, type = "response")
#ROCR
library(ROCR)
ROCRpred= prediction(model2_predict, cv$Class)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize =TRUE, print.cutoffs.at =seq(0.1,0.1), text.adj=c(-0.2, 1.7))

#
table(cv$Class, model2_predict > 0.5)

model2_test_accuracy = (2994+136)/(2994+6+12+136)
model2_test_accuracy

#Accuracy seems High, lets check precision and recall

model2_precision = 136/(136+6)
model2_precision
model2_recall = 136/(136+12)
model2_recall
#
model2_F_measure = 2*model2_precision*model2_recall/(model2_precision+model2_recall)
model2_F_measure
#



#Model2 seems to be good model


#__________________________#

#Random Forest
library(randomForest)
train$Class <- factor(train$Class)
model3 = randomForest(Class ~ ., data=train, ntree = 2000, nodesize=20)
cv$Class = factor(cv$Class)
model3_predict = predict(model3, cv)
confusionMatrix(cv$Class, model3_predict)
#



#Decision trees
model4 = rpart(Class ~., data=train, method = "class", minbucket =20)
prp(model4)
model4_predict = predict(model4, cv, type="class")
confusionMatrix(cv$Class, model4_predict)
###################
