# Count missing values per column
colSums(is.na(SymptomData))

# Total missing values in dataset
sum(is.na(SymptomData))

# Function to impute mode
impute_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply imputation if missing values exist
SymptomData <- SymptomData %>%
  mutate(across(everything(), ~ ifelse(is.na(.), impute_mode(.), .)))

# Create numeric version for ML algorithms(neural networks, glmnet)
SymptomData_num <- SymptomData %>%
  mutate(across(-label, ~ as.numeric(as.character(.))))

