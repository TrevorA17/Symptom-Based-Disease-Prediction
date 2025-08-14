# plumber.R

library(plumber)
library(caret)

# --- Load model and metadata ---
repo_path <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Load trained Naive Bayes model
loaded_nb_model <- readRDS(file.path(repo_path, "models", "saved_nb_model.rds"))

# Load metadata (column names, types, factor levels, etc.)
metadata <- readRDS(file.path(repo_path, "models", "model_metadata.rds"))

# --- Helper: Prepare new data using metadata ---
prepare_new_data <- function(input_list, metadata) {
  # Convert list to data frame
  new_df <- as.data.frame(input_list, stringsAsFactors = FALSE)
  
  # Ensure all expected columns exist
  for (col in names(metadata$types)) {
    if (!col %in% names(new_df)) {
      new_df[[col]] <- NA
    }
  }
  
  # Match column order
  new_df <- new_df[names(metadata$types)]
  
  # Coerce data types
  for (col in names(metadata$types)) {
    if (metadata$types[[col]] == "numeric") {
      new_df[[col]] <- as.numeric(new_df[[col]])
    } else if (metadata$types[[col]] == "factor") {
      new_df[[col]] <- factor(new_df[[col]], levels = metadata$levels[[col]])
    }
  }
  
  return(new_df)
}

#* @apiTitle Symptom-Based Disease Prediction API
#* @apiDescription Predict disease based on symptom inputs.

# Example symptoms params (replace with your actual columns)
#* @param Symptom1 First symptom (factor)
#* @param Symptom2 Second symptom (factor)
#* @param Symptom3 Third symptom (factor)
#* @param Age Age of the patient (numeric)
#* @param Gender Gender (factor)

#* @post /predict
function(Symptom1, Symptom2, Symptom3, Age, Gender) {
  # Create input list
  input_list <- list(
    Symptom1 = Symptom1,
    Symptom2 = Symptom2,
    Symptom3 = Symptom3,
    Age = Age,
    Gender = Gender
  )
  
  # Prepare new data with correct structure
  new_data <- prepare_new_data(input_list, metadata)
  
  # Make prediction
  prediction <- predict(loaded_nb_model, newdata = new_data)
  
  return(list(prediction = prediction))
}
