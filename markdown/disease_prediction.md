Symptom-Based Disease Prediction
================
Trevor Okinda
2025

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Understanding the Dataset (Exploratory Data Analysis
  (EDA))](#understanding-the-dataset-exploratory-data-analysis-eda)
  - [Loading the Dataset](#loading-the-dataset)
    - [Source:](#source)
    - [Reference:](#reference)
- [Exploratory Data Analysis](#exploratory-data-analysis)
  - [Load dataset](#load-dataset)

# Student Details

|                       |                                          |
|-----------------------|------------------------------------------|
| **Student ID Number** | 134780                                   |
| **Student Name**      | Trevor Okinda                            |
| **BBIT 4.2 Group**    | C                                        |
| **Project Name**      | A Symptom-Based disease prediction model |

# Setup Chunk

**Note:** the following KnitR options have been set as the global
defaults: <BR>
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

# Understanding the Dataset (Exploratory Data Analysis (EDA))

## Loading the Dataset

### Source:

The dataset that was used can be downloaded here: *\<<a
href="https://www.kaggle.com/datasets/miltonmacgyver/symptom-based-disease-prediction-dataset\"
class="uri">https://www.kaggle.com/datasets/miltonmacgyver/symptom-based-disease-prediction-dataset\</a>\>*

### Reference:

*\*MiltonMacGyver. (n.d.). Symptom-Based Disease Prediction Dataset
\[Data set\]. Kaggle.
<https://www.kaggle.com/datasets/miltonmacgyver/symptom-based-disease-prediction-dataset>*
\>  

Refer to the APA 7th edition manual for rules on how to cite datasets:
<https://apastyle.apa.org/style-grammar-guidelines/references/examples/data-set-references>\*

# Exploratory Data Analysis

## Load dataset

``` r
# Load libraries
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
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
```

    ## 'data.frame':    4998 obs. of  16 variables:
    ##  $ fever           : Factor w/ 2 levels "0","1": 2 2 2 2 2 1 2 2 1 2 ...
    ##  $ cough           : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 1 2 1 2 ...
    ##  $ headache        : Factor w/ 2 levels "0","1": 2 2 2 2 2 1 2 2 1 2 ...
    ##  $ nausea          : Factor w/ 2 levels "0","1": 2 1 2 2 2 1 1 1 1 2 ...
    ##  $ vomiting        : Factor w/ 2 levels "0","1": 1 2 2 1 2 1 2 1 1 1 ...
    ##  $ fatigue         : Factor w/ 2 levels "0","1": 1 1 2 2 1 2 2 2 1 2 ...
    ##  $ sore_throat     : Factor w/ 2 levels "0","1": 1 2 1 1 1 2 1 2 1 2 ...
    ##  $ chills          : Factor w/ 2 levels "0","1": 2 2 1 1 2 1 2 1 1 1 ...
    ##  $ body_pain       : Factor w/ 2 levels "0","1": 2 1 1 1 2 2 1 1 2 2 ...
    ##  $ loss_of_appetite: Factor w/ 2 levels "0","1": 2 1 2 1 1 1 2 1 1 1 ...
    ##  $ abdominal_pain  : Factor w/ 2 levels "0","1": 2 2 2 1 2 1 2 2 1 1 ...
    ##  $ diarrhea        : Factor w/ 2 levels "0","1": 1 1 2 1 2 1 2 2 1 1 ...
    ##  $ sweating        : Factor w/ 2 levels "0","1": 1 2 2 2 2 1 1 1 2 1 ...
    ##  $ rapid_breathing : Factor w/ 2 levels "0","1": 1 2 1 1 1 2 1 1 1 2 ...
    ##  $ dizziness       : Factor w/ 2 levels "0","1": 1 1 2 1 2 1 2 2 1 1 ...
    ##  $ label           : Factor w/ 3 levels "Malaria","Pneumonia",..: 3 1 3 1 3 2 3 3 1 2 ...

``` r
summary(SymptomData)
```

    ##  fever    cough    headache nausea   vomiting fatigue  sore_throat chills  
    ##  0:1161   0:2313   0:1950   0:2968   0:3162   0:1676   0:3137      0:2338  
    ##  1:3837   1:2685   1:3048   1:2030   1:1836   1:3322   1:1861      1:2660  
    ##                                                                            
    ##  body_pain loss_of_appetite abdominal_pain diarrhea sweating rapid_breathing
    ##  0:2179    0:2027           0:2696         0:3148   0:2007   0:2621         
    ##  1:2819    1:2971           1:2302         1:1850   1:2991   1:2377         
    ##                                                                             
    ##  dizziness       label     
    ##  0:2807    Malaria  :1666  
    ##  1:2191    Pneumonia:1666  
    ##            Typhoid  :1666

``` r
# Frequency of each disease label
table(SymptomData$label)
```

    ## 
    ##   Malaria Pneumonia   Typhoid 
    ##      1666      1666      1666
