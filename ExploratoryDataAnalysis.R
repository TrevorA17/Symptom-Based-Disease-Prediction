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

library(ggplot2)

ggplot(SymptomData, aes(x = label)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Disease Distribution", x = "Disease", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Convert to numeric for counting
symptom_numeric <- SymptomData %>%
  mutate(across(-label, ~ as.numeric(as.character(.))))

symptom_counts <- colSums(symptom_numeric[ , -ncol(symptom_numeric)] == 1)

symptom_df <- data.frame(
  Symptom = names(symptom_counts),
  Count = as.numeric(symptom_counts)
)

ggplot(symptom_df, aes(x = reorder(Symptom, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Symptom Frequency", x = "Symptom", y = "Count")

# Example: Fever vs Disease
ggplot(SymptomData, aes(x = label, fill = fever)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Fever Presence by Disease",
       y = "Proportion") +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "red"))
# Choose a few symptoms to display
selected_symptoms <- c("fever", "cough", "headache")

SymptomData %>%
  pivot_longer(cols = all_of(selected_symptoms), names_to = "Symptom", values_to = "Value") %>%
  ggplot(aes(x = label, fill = Value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Symptom) +
  labs(title = "Symptom Presence by Disease", y = "Proportion") +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "darkgreen"))

library(tidyverse)

label_col <- "label"

# Convert symptom columns to numeric (keep label as factor)
symptom_numeric <- SymptomData %>%
  mutate(across(setdiff(names(SymptomData), label_col), ~ as.numeric(as.character(.))))

# Group by label and calculate mean (proportion of '1's)
symptom_summary <- symptom_numeric %>%
  group_by(.data[[label_col]]) %>%
  summarise(across(setdiff(names(symptom_numeric), label_col), mean), .groups = "drop") %>%
  pivot_longer(-.data[[label_col]], names_to = "Symptom", values_to = "Proportion")

# Heatmap
ggplot(symptom_summary, aes(x = Symptom, y = .data[[label_col]], fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Symptom Presence by Disease",
       x = "Symptom",
       y = "Disease",
       fill = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

