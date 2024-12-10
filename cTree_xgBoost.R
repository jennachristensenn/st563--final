
library(rpart.plot)
library(tidyverse)
library(tidymodels)
library(rpart)
library(caret)
library(tree)
library(gbm)

diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

diabetes_data <- diabetes_data %>%
  filter(BMI < 50)

# converting into factors
diab_data <- diabetes_data |>
  mutate(diab_bin = factor(Diabetes_binary, levels = c(0, 1),
                           labels = c("no_diabetes", "diabetes")),
         highBP = factor(HighBP, levels = c(0, 1), 
                         labels = c("no", "yes")),
         highCol = factor(HighChol, levels = c(0, 1), 
                          labels = c("No", "Yes")),
         colCheck = factor(CholCheck, levels = c(0, 1),
                           labels = c("No", "Yes")),
         heartDA = factor(HeartDiseaseorAttack, levels = c(0, 1), 
                          labels = c("No", "Yes")),
         heavyAlc = factor(HvyAlcoholConsump, levels = c(0, 1),
                           labels = c("No", "Yes")),
         genHlth = factor(GenHlth, levels  = c(1, 2, 3, 4, 5), 
                          labels = c("excellent", "very_good", "good", "fair", "poor")),
         sex = factor(Sex, levels = c(0, 1), 
                      labels = c("female", "male"))) |>
  select(diab_bin, highBP, highCol, colCheck, heartDA, heavyAlc, genHlth, sex, Age) 
diab_data

set.seed(123)

# Create a 70-30 split for final evaluation (this test set is kept aside)
train_idx <- sample(seq_len(nrow(diab_data)), size = 0.7 * nrow(diab_data))
training_data <- diab_data[train_idx, ]
testing_data  <- diab_data[-train_idx, ]


# Set up 5-fold cross-validation using trainControl
train_control <- trainControl(method = "cv", 
                              number = 5, 
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              savePredictions = TRUE)

# Train the model using rpart with 5-fold CV
training_data$diab_bin <- relevel(training_data$diab_bin, ref = "diabetes")



# Classification tree

set.seed(123)
model_cv <- train(diab_bin ~ ., 
                  data = training_data,
                  method = "rpart",
                  metric = "ROC",
                  trControl = train_control,
                  tuneLength = 10)
model_cv

# Plot the performance 
plot(model_cv)

# Predict on the test set
testing_data$diab_bin <- relevel(testing_data$diab_bin, ref = "diabetes")
predictions <- predict(model_cv, newdata = testing_data)
confusion_mat <- confusionMatrix(predictions, testing_data$diab_bin, positive = "diabetes")
confusion_mat


# boosting 

# Define a grid of tuning parameters for GBM
gbm_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = c(50, 100, 150),
  shrinkage = c(0.01, 0.1), 
  n.minobsinnode = c(10, 20) 
)

# Train the GBM model with bernoulli distribution
set.seed(123) 
model_gbm <- train(
  diab_bin ~ .,
  data = training_data,
  method = "gbm",
  trControl = train_control,
  tuneGrid = gbm_grid,
  metric = "ROC",
  distribution = "bernoulli",
  verbose = FALSE
)

# Review the results of tuning
print(model_gbm)
plot(model_gbm)

# Predict on the test data
predictions_gbm <- predict(model_gbm, newdata = testing_data)

# Evaluate model performance on the test set
confusion_mat_gbm <- confusionMatrix(predictions_gbm, testing_data$diab_bin, positive = "diabetes")
print(confusion_mat_gbm)

# (Optional) Print ROC, Sensitivity, Specificity
print(confusion_mat_gbm$byClass)