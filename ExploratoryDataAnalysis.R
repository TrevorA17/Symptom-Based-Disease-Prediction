# Load libraries
library(tidyverse)

# Choose and load dataset with correct column types
file_path <- file.choose()

SymptomData <- read.csv(file_path, colClasses = c(
  fever = "factor",
  cough = "factor",
  headache = "factor",
  nausea = "factor",
  vomiting = "factor",
  fatigue = "factor",
  sore_throat = "factor",
  chills = "factor",
  body_pain = "factor",
  loss_of_appetite = "factor",
  abdominal_pain = "factor",
  diarrhea = "factor",
  sweating = "factor",
  rapid_breathing = "factor",
  dizziness = "factor",
  label = "factor" # Target variable
))

# Quick check
str(SymptomData)
summary(SymptomData)

# Frequency of each disease label
table(SymptomData$label)

# Frequency of each symptom being present (1) or absent (0)
symptom_freq <- sapply(SymptomData[ , -ncol(SymptomData)], function(col) table(col))
symptom_freq

# Convert factors to numeric (0/1) for calculation
symptom_numeric <- SymptomData %>%
  mutate(across(-label, ~ as.numeric(as.character(.))))

# Mean presence of each symptom
colMeans(symptom_numeric[ , -ncol(symptom_numeric)])

# Distribution for each symptom
summary(symptom_numeric[ , -ncol(symptom_numeric)])

# Visual distribution for one example symptom
library(ggplot2)
ggplot(SymptomData, aes(x = fever)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Fever Symptom")

# Chi-square test for fever vs disease
chisq.test(table(SymptomData$fever, SymptomData$label))

install.packages("DescTools")
# Cramér's V for all symptoms vs label
library(DescTools)
cramers_v <- sapply(SymptomData[ , -ncol(SymptomData)], function(symptom) {
  CramerV(table(symptom, SymptomData$label))
})
cramers_v
