# Load the data
dataset <- read.csv("pima-indians-diabetes.csv")

library(glmnet)
library(tidyverse)
library(caret)

# Split the data into training and test set
set.seed(1)
training.samples <- dataset$X1 %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- dataset[training.samples, ]
test.data <- dataset[-training.samples, ]

# Predictor variables
x <- model.matrix(X1~., train.data)[,-1]

# Outcome variable
y <- train.data$X1

# Fit the lasso penalized regression model
# Find the best lambda using cross-validation
set.seed(1) 
cv1 <- cv.glmnet(x, y, alpha = 1, family = "binomial")

#lasso regression
model1 <- glmnet(x, y, alpha = 1, lambda = cv1$lambda.min,
                 family = "binomial")

# Make predictions on the test data
x.test <- model.matrix(test.data$X1~., test.data)[,-1]
probabilities1 <- model1 %>% predict(newx = x.test)
predicted.classes1 <- ifelse(probabilities1 > 0.5, 1, 0)

# Model accuracy
library(MLmetrics)
LogLoss(predicted.classes1,test.data$X1)
RMSE(predicted.classes1,test.data$X1)
AUC(predicted.classes1, test.data$X1)
F1_Score(predicted.classes1, test.data$X1)

# Fit the lasso penalized regression model
# Find the best lambda using cross-validation
set.seed(1) 
cv2 <- cv.glmnet(x, y, alpha = 0, family = "binomial")

#lasso regression
model2 <- glmnet(x, y, alpha = 0, lambda = cv2$lambda.min,
                 family = "binomial")

# Make predictions on the test data
x.test <- model.matrix(test.data$X1~., test.data)[,-1]
probabilities2 <- model2 %>% predict(newx = x.test)
predicted.classes2 <- ifelse(probabilities2 > 0.5, 1, 0)

# Model accuracy
library(MLmetrics)
LogLoss(predicted.classes2,test.data$X1)
RMSE(predicted.classes2,test.data$X1)
AUC(predicted.classes2, test.data$X1)
F1_Score(predicted.classes2, test.data$X1)