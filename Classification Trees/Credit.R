library(tidyverse)
library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)


setwd("C:/Users/clear/Workspace/GitHub/MachineLearning/Classification Trees")

credit <- read_csv("Credit_data.csv")
glimpse(credit)

n <- nrow(credit)

n_train <- round(0.8 * n)

set.seed(123)

train_indices <- sample(1:n, n_train)

train <- credit[train_indices, ]
test <- credit[-train_indices, ]
test$default <- as.factor(test$default)

train <- train %>% select(-X1)
test <- test %>% select(-X1)

model <- rpart(default ~ ., data = train, method = "class")

print(model)
rpart.plot(model, yesno = 2, type = 0, extra = 0)

class_prediction <- predict(model, newdata = test, type = "class")

confusionMatrix(data = class_prediction, reference = test$default)

# Train a gini-based model
credit_model1 <- rpart(formula = default ~ ., 
                       data = train, 
                       method = "class",
                       parms = list(split = "gini"))

# Train an information-based model
credit_model2 <- rpart(formula = default ~ ., 
                       data = train, 
                       method = "class",
                       parms = list(split = "information"))

# Generate predictions on the validation set using the gini model
pred1 <- predict(object = credit_model1, 
                 newdata = test,
                 type = "class")    

# Generate predictions on the validation set using the information model
pred2 <- predict(object = credit_model2, 
                 newdata = test,
                 type = "class")

# Compare classification error
# Classification error is the fraction of incorrectly classified instances
ce(actual = test$default, predicted = pred1)

ce(actual = test$default, predicted = pred2)  
