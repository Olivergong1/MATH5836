#Q1
# Load the data
dataset <- read.csv("pima-indians-diabetes.csv")

library(tidyverse)
library(caret)
# Split the data into training and test set
set.seed(1)
training.samples <- dataset$X1 %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- dataset[training.samples, ]
test.data <- dataset[-training.samples, ]

# Fit the model
model <- glm( X1~., data = dataset, family = binomial)

# Summarize the model
summary(model)

# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
library(MLmetrics)
LogLoss(predicted.classes,test.data$X1)
RMSE(predicted.classes,test.data$X1)

#Q2
# roc curve, Precision-Recall curve and auc
library(precrec)
roc_obj <- evalmod(scores = test.data$X1,labels = predicted.classes)
auc(roc_obj)
autoplot(roc_obj)
F1_Score(predicted.classes,test.data$X1, positive = 1)

#Q3
#Reduce imbalance by over sampling
library(ROSE)
dataset_balanced <- ovun.sample(X1~., data = dataset, seed = 1)$data
table(dataset_balanced$X1)

# Split the data into training and test set
set.seed(1)
training.samples1 <- dataset_balanced$X1 %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data1  <- dataset_balanced[training.samples1, ]
test.data1 <- dataset_balanced[-training.samples1, ]

# Fit the model
model1 <- glm( X1~., data = dataset_balanced, family = binomial)

# Summarize the model
summary(model1)

# Make predictions
probabilities1 <- model1 %>% predict(test.data1, type = "response")
predicted.classes1 <- ifelse(probabilities1 > 0.5, 1, 0)

# roc curve, Precision-Recall curve and auc
roc_obj1 <- evalmod(scores = test.data1$X1,labels = predicted.classes1)
auc(roc_obj1)
autoplot(roc_obj1)
F1_Score(predicted.classes1,test.data1$X1, positive = 1)

#Q4
# Define training control
set.seed(1)
train.control <- trainControl(method = "cv", number = 10)

# Train the model
dataset$X1 <- as.factor(dataset$X1)
model2 <- train(X1~., data = dataset, method = "glm",
                trControl = train.control)

# Summarize the results
print(model2)