# 0. Libraries

library(lubridate)  # for convenient date handling (mdy)
library(ranger)     # random forest regression
library(xgboost)    # gradient boosting regression
library(ggplot2)    # plotting

set.seed(123)       # for reproducibility

# 1. Load data

df_train <- read.csv("df_train.csv", stringsAsFactors = FALSE)
df_test  <- read.csv("df_test.csv",  stringsAsFactors = FALSE)

# 2. Preprocessing

df_train$date <- mdy(df_train$date)
df_test$date  <- mdy(df_test$date)

df_train$day_in_week <- factor(df_train$day_in_week)
df_test$day_in_week  <- factor(df_test$day_in_week)

# Train dummies

train_dummies <- as.data.frame(
  model.matrix(~ day_in_week - 1, data = df_train)
)

# Test dummies

test_dummies <- as.data.frame(
  model.matrix(~ day_in_week - 1, data = df_test)
)

# Combine with original data and remove original factor column

df_train <- cbind(df_train, train_dummies)
df_test  <- cbind(df_test,  test_dummies)

df_train$day_in_week <- NULL
df_test$day_in_week  <- NULL

# 3. Split into features (X) and target (y)

# Target variable
train_y <- df_train[["power_consumption"]]
test_y  <- df_test[["power_consumption"]]

# Feature data frames (drop power_consumption and date)
train_X <- df_train[, !(names(df_train) %in% c("power_consumption", "date"))]
test_X  <- df_test[,  !(names(df_test)  %in% c("power_consumption", "date"))]

# For xgboost, we need numeric matrices
train_matrix <- as.matrix(train_X)
test_matrix  <- as.matrix(test_X)

# 4. Helper: RMSE function

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# 5. Train models

# 5.1 Linear Regression 

lm_model <- lm(
  power_consumption ~ . - date,
  data = df_train
)

# 5.2 Random Forest using ranger

rf_model <- ranger(
  formula    = power_consumption ~ . - date,
  data       = df_train,
  num.trees  = 800,                      # more trees for better performance
  mtry       = floor(sqrt(ncol(train_X))), # number of variables tried at each split
  min.node.size = 5,
  seed       = 123
)

# 5.3 XGBoost

xgb_model <- xgboost(
  data        = train_matrix,
  label       = train_y,
  objective   = "reg:squarederror",
  max_depth   = 6,       # can tune
  eta         = 0.1,     # learning rate
  nrounds     = 400,     # number of boosting iterations (can tune)
  subsample   = 0.8,
  colsample_bytree = 0.8,
  nthread     = 2,
  verbose     = 0
)

# 6. Predictions on test set

# 6.1 Linear Regression predictions

lm_pred <- predict(lm_model, newdata = df_test)

# 6.2 Random Forest predictions

rf_pred <- predict(rf_model, data = df_test)$predictions

# 6.3 XGBoost predictions

xgb_pred <- predict(xgb_model, newdata = test_matrix)

# 7. Calculate RMSE for each model

rmse_lm  <- rmse(test_y, lm_pred)
rmse_rf  <- rmse(test_y, rf_pred)
rmse_xgb <- rmse(test_y, xgb_pred)

rmse_values <- c(
  LinearRegression = rmse_lm,
  RandomForest     = rmse_rf,
  XGBoost          = rmse_xgb
)

# Lowest RMSE (should be < 450 kW for the task requirement)

selected_rmse <- min(rmse_values)
selected_rmse

# Name of the best model

best_model_name <- names(which.min(rmse_values))
best_model_name

# 8. Choose predictions from best model

if (best_model_name == "LinearRegression") {
  best_pred <- lm_pred
} else if (best_model_name == "RandomForest") {
  best_pred <- rf_pred
} else { # "XGBoost"
  best_pred <- xgb_pred
}

# 9. Power consumption over time plot

plot_df <- df_test
plot_df$predicted_power <- best_pred

ggplot(data = plot_df, aes(x = date)) +
  geom_line(aes(y = power_consumption, colour = "Actual")) +
  geom_line(aes(y = predicted_power,   colour = "Predicted")) +
  labs(
    title = paste("Actual vs Predicted Daily Power Consumption (", 
                  best_model_name, ")", sep = ""),
    x = "Date",
    y = "Daily Power Consumption (kW)",
    colour = ""
  ) +
  theme_minimal()

# 10. Trend similarity

cor_value <- cor(test_y, best_pred)
cor_value

trend_similarity <- if (cor_value > 0.5) "Yes" else "No"
trend_similarity
