# plumber_nb_api.R

# Load necessary libraries
library(plumber)
library(caret)
library(e1071) # for Naive Bayes if needed

# Load the saved Naive Bayes model
loaded_nb_model <- readRDS("./models/saved_nb_model.rds")

# Load your original training data (needed for column structure)
train_data <- readRDS("./models/training_data.rds")

# Helper function to align incoming data with training structure
prepare_new_data <- function(new_data, training_data) {
  # Remove label column
  train_features <- training_data[, setdiff(names(training_data), "label")]
  
  # Add missing columns as NA
  for (col in names(train_features)) {
    if (!col %in% names(new_data)) {
      new_data[[col]] <- NA
    }
  }
  
  # Reorder columns
  new_data <- new_data[, names(train_features)]
  
  # Match types
  for (col in names(train_features)) {
    if (is.factor(train_features[[col]])) {
      new_data[[col]] <- factor(new_data[[col]], levels = levels(train_features[[col]]))
    } else {
      new_data[[col]] <- as.numeric(new_data[[col]])
    }
  }
  
  return(new_data)
}

#* Predict using Naive Bayes model
#* @post /predict
#* @param fever Numeric (0 or 1)
#* @param cough Numeric (0 or 1)
#* @param headache Numeric (0 or 1)
#* @param nausea Numeric (0 or 1)
#* @param vomiting Numeric (0 or 1)
#* @param fatigue Numeric (0 or 1)
#* @param sore_throat Numeric (0 or 1)
#* @param chills Numeric (0 or 1)
#* @param body_pain Numeric (0 or 1)
#* @param loss_of_appetite Numeric (0 or 1)
#* @param abdominal_pain Numeric (0 or 1)
#* @param diarrhea Numeric (0 or 1)
#* @param sweating Numeric (0 or 1)
#* @param rapid_breathing Numeric (0 or 1)
#* @param dizziness Numeric (0 or 1)
#* @response 200 Returns predicted class
predict_symptoms <- function(
    fever, cough, headache, nausea, vomiting, fatigue,
    sore_throat, chills, body_pain, loss_of_appetite,
    abdominal_pain, diarrhea, sweating, rapid_breathing, dizziness
) {
  # Create dataframe from inputs
  new_data <- data.frame(
    fever = as.numeric(fever),
    cough = as.numeric(cough),
    headache = as.numeric(headache),
    nausea = as.numeric(nausea),
    vomiting = as.numeric(vomiting),
    fatigue = as.numeric(fatigue),
    sore_throat = as.numeric(sore_throat),
    chills = as.numeric(chills),
    body_pain = as.numeric(body_pain),
    loss_of_appetite = as.numeric(loss_of_appetite),
    abdominal_pain = as.numeric(abdominal_pain),
    diarrhea = as.numeric(diarrhea),
    sweating = as.numeric(sweating),
    rapid_breathing = as.numeric(rapid_breathing),
    dizziness = as.numeric(dizziness)
  )
  
  # Align new data
  new_data_ready <- prepare_new_data(new_data, train_data)
  
  # Predict
  prediction <- predict(loaded_nb_model, newdata = new_data_ready)
  
  # Return prediction
  return(list(prediction = as.character(prediction)))
}
