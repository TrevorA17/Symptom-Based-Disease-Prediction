# plumber_nb_api.R

# Load libraries
library(plumber)
library(e1071) # Naive Bayes

# Load the saved Naive Bayes model
loaded_nb_model <- readRDS("./models/saved_nb_model.rds")

# Load model metadata (column names, types, levels)
model_metadata <- readRDS("./models/model_metadata.rds")

# API Info
#* @apiTitle Symptom-Based Naive Bayes Prediction API
#* @apiDescription Predicts disease likelihood based on symptoms.

# Helper function to prepare incoming data
prepare_new_data <- function(new_data, metadata) {
  # Ensure all expected columns exist
  for (col in metadata$columns) {
    if (!col %in% names(new_data)) {
      new_data[[col]] <- NA
    }
  }
  
  # Reorder columns to match training
  new_data <- new_data[, metadata$columns, drop = FALSE]
  
  # Match data types
  for (col in metadata$columns) {
    if (metadata$types[[col]] == "factor") {
      new_data[[col]] <- factor(new_data[[col]], levels = metadata$levels[[col]])
    } else {
      new_data[[col]] <- as.numeric(new_data[[col]])
    }
  }
  
  return(new_data)
}

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

#* @get /predict
predict_symptoms <- function(
    fever, cough, headache, nausea, vomiting, fatigue,
    sore_throat, chills, body_pain, loss_of_appetite,
    abdominal_pain, diarrhea, sweating, rapid_breathing, dizziness
) {
  
  # Create data frame from inputs
  to_be_predicted <- data.frame(
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
  
  # Align incoming data to match model training structure
  to_be_predicted_ready <- prepare_new_data(to_be_predicted, model_metadata)
  
  # Predict using the Naive Bayes model
  prediction <- predict(loaded_nb_model, newdata = to_be_predicted_ready)
  
  # Return prediction as plain text
  return(list(prediction = as.character(prediction)))
}
