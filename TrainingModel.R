library(caret)

set.seed(123)  # For reproducibility

# 80/20 split
train_index <- createDataPartition(SymptomData$label, p = 0.8, list = FALSE)
train_data <- SymptomData[train_index, ]
test_data  <- SymptomData[-train_index, ]

dim(train_index)
dim(train_data)

ctrl_boot <- trainControl(method = "boot", number = 50)  # 50 bootstrap resamples
print(ctrl_boot)

ctrl_cv <- trainControl(method = "cv", number = 10)
print(ctrl_cv)

# Logistic Regression
# Naive Bayes
model_nb <- train( label ~ .,  data = train_data, method = "naive_bayes", trControl = ctrl_cv )

# Decision Tree
model_tree <- train( label ~ .,  data = train_data, method = "rpart", trControl = ctrl_cv )

# Random Forest
model_rf <- train( label ~ .,  data = train_data, method = "rf", trControl = ctrl_cv )
