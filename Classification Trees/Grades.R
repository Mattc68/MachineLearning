library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

setwd("C:/Users/clear/Workspace/GitHub/MachineLearning/Classification Trees")

grades <- read_csv("Grade_data.csv")

set.seed(123)

assignment <- sample(1:3, nrow(grades), prob = c(0.7, 0.15, 0.15), replace = T)

train <- grades[assignment == 1, ]
val <- grades[assignment == 2, ]
test <- grades[assignment == 3, ]

model <- rpart(final_grade ~ ., train, method = "anova")

rpart.plot(model, type = 0, extra = 0, yesno = 2)

pred <- predict(model, test)

RMSE(pred, test$final_grade)

plotcp(model)
print(model$cptable)

opt_index <- which.min(model$cptable[, "xerror"])
cp_opt <- model$cptable[opt_index, "CP"]

model_opt <- prune(tree = model, cp = cp_opt)

rpart.plot(x = model_opt, type = 0, extra = 0, yesno = 2)

minsplit <- seq(1, 4, 1)
minsplit

maxdepth <- seq(1, 6, 1)
maxdepth

hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)
hyper_grid

grade_models <- list()

for(i in 1:nrow(hyper_grid)){
  grade_models[[i]] <- rpart(final_grade ~ ., 
                             train, method = "anova", 
                             minsplit = hyper_grid$minsplit[i],
                             maxdepth = hyper_grid$maxdepth[i])
}

rmse_values <- c()

for(i in 1:length(grade_models)){
  model <- grade_models[[i]]
  pred <- predict(model, val)
  rmse_values <- c(rmse_values, RMSE(val$final_grade, pred))
}

best_model <- grade_models[[which.min(rmse_values)]]
best_model$control

pred <- predict(best_model, test)
rmse(test$final_grade, pred)
