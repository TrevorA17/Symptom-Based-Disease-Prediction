# Ensure the 'models' directory exists
if (!dir.exists("./models")) {
  dir.create("./models", recursive = TRUE)
}

# Save the best model (Naive Bayes) to a file
repo_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
saveRDS(model_nb, file.path(repo_path, "models", "saved_nb_model.rds"))

# Load the saved Naive Bayes model (when needed)
loaded_nb_model <- readRDS("./models/saved_nb_model.rds")

# Helper function to align new data to training data structure
prepare_new_data <- function(new_data, training_data) {
  # Remove label from training data
  train_features <- training_data[, setdiff(names(training_data), "label")]
  
  # Ensure all expected columns are present in new_data
  for (col in names(train_features)) {
    if (!col %in% names(new_data)) {
      new_data[[col]] <- NA  # fill missing columns with NA
    }
  }
  
  # Reorder columns to match training data
  new_data <- new_data[, names(train_features)]
  
  # Convert column types to match training
  for (col in names(train_features)) {
    if (is.factor(train_features[[col]])) {
      new_data[[col]] <- factor(new_data[[col]], levels = levels(train_features[[col]]))
    } else {
      new_data[[col]] <- as.numeric(new_data[[col]])
    }
  }
  
  return(new_data)
}

# Example usage
new_data_input <- data.frame(
  fever = 1,
  cough = 0,
  headache = 1,
  nausea = 0,
  vomiting = 0,
  fatigue = 1,
  sore_throat = 0,
  chills = 1,
  body_pain = 1,
  loss_of_appetite = 0,
  abdominal_pain = 0,
  diarrhea = 0,
  sweating = 1,
  rapid_breathing = 0,
  dizziness = 0
)

# Prepare data
new_data_ready <- prepare_new_data(new_data_input, train_data)

# Predict
predictions <- predict(loaded_nb_model, newdata = new_data_ready)
print(predictions)
