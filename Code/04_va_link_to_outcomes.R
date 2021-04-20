############ ---------------------------
##
## Script name: 03_value_added_estimation.R
##
## Purpose of script: Calculate our teacher level value added scores. 
##
## Author: Roy McKenzie
##
## Date Created: 4-10-2021
##
## ---------------------------

### 1. Load Libraries and utility functions
library(tidyverse)
library(haven)
library(dplyr)
library(lmtest)
library(janitor)
library(stringr)
source("utils.R")

### 2. Load Data
analytic_dataset <- read_rds("../Output/analytic_dataset.rds") %>%
  filter(
    subject == "Math" | subject == "English"
  )
analytic_cohort <- read_rds("../Output/analytic_cohort.rds") %>%
  filter(SID %in% analytic_dataset$SID)

va_scores <- read_rds("../Output/va_output.rds")

### 3. Merge Selected VA Score with Analytic Cohort
analytic_dataset <- left_join(analytic_dataset, va_scores, by = c("TID", "subject", "FRESH_COHORT_YEAR"))

### 4. Run Analysis
outcomes <- c("nAPCourses4yr", "dEarnRegDip4yr", "dFreshOnTrack", "highACT_EQUIV",
              "gradCumCoreGPA", "nHonorsCourses4yr")
controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend", 
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped",
              "class_MATH_Z", "class_READ_Z", "class_age", "class_rnoAttend",
              "class_n_infractions_grade_8", "class_rnoCoreGpa", "class_size",
              "I(MATH_Z^2)", "I(READ_Z^2)", "I(age^2)", "I(rnoAttend^2)",
              "I(n_infractions_grade_8^2)", "I(rnoCoreGpa^2)",
              "I(MATH_Z^3)", "I(READ_Z^3)", "I(age^3)", "I(rnoAttend^3)",
              "I(n_infractions_grade_8^3)", "I(rnoCoreGpa^3)",
              "I(class_MATH_Z^2)", "I(class_READ_Z^2)", "I(class_age^2)", "I(class_rnoAttend^2)",
              "I(class_n_infractions_grade_8^2)", "I(class_rnoCoreGpa^2)",
              "I(class_MATH_Z^3)", "I(class_READ_Z^3)", "I(class_age^3)", "I(class_rnoAttend^3)",
              "I(class_n_infractions_grade_8^3)", "I(class_rnoCoreGpa^3)",
              "I(class_size^2)", "I(class_size^3)")
controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend",
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped",
              "I(MATH_Z^2)", "I(READ_Z^2)", "I(age^2)", "I(rnoAttend^2)",
              "I(n_infractions_grade_8^2)", "I(rnoCoreGpa^2)",
              "I(MATH_Z^3)", "I(READ_Z^3)", "I(age^3)", "I(rnoAttend^3)",
              "I(n_infractions_grade_8^3)", "I(rnoCoreGpa^3)")
controls <- paste(controls, collapse = " + ")
subjects <- c("Math", "English")

top_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model2, .95)
top_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model2, .95)
low_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model2, .05)
low_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model2, .05)

results <- data.frame()
for(outcome in outcomes) {
  results_this_outcome <- c()
    
  ### Indep. Var 1 - Grade Effect (in each subject)
  ### Q: Am I doing this demeaning right?
  ### Q: Clustering at the class level?
  for(current_sub in subjects) {
    temp_data <- analytic_dataset %>% 
      filter(
        subject == current_sub
      )
    data_with_demeaned_outcome <- dresid(temp_data, outcome, controls)
    reg1 <- lm(paste0(outcome, "_dresid ~ va_model2"), data = data_with_demeaned_outcome)
    test <- coeftest(reg1, plm::vcovHC(reg1, type = "HC3", cluster = c("funit")))
    results_this_outcome <- c(results_this_outcome, 
                              paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                              " [", round(test[2, 4], 3), "]"))
  }
  ### Indep. Var 2 _ Having a particularly high/low value add
  ### Generate variable indicating if a student had a teacher in the top 5% of VA for that subject
  ### TODO: Add descriptives on this variable
  ### TODO: Check, Do we pool for residualizing outcomes?
  analytic_dataset_resid <- dresid(analytic_dataset, outcome, controls)
  temp_data <- analytic_dataset %>%
    select(SID, va_model2, subject) %>%
    group_by(SID, subject) %>%
    summarize(
      high_va = ifelse((va_model2 >= top_math_cutoff & subject == "Math") |
                         (va_model2 >= top_eng_cutoff & subject == "Eng"), 1, 0)
    )
  analytic_cohort_resid <- analytic_dataset_resid %>%
    left_join(temp_data, by = c("SID", "subject"))
  reg2 <- lm(paste0(outcome, "_dresid ~ high_va"), data = analytic_cohort_resid)
  test <- coeftest(reg2, plm::vcovHC(reg2, type = "HC3", cluster = c("funit")))
  results_this_outcome <- c(results_this_outcome, 
                            paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                                   " [", round(test[2, 4], 3), "]"))
  
  
  results_this_outcome_df <- data.frame(t(results_this_outcome), row.names = outcome)
  colnames(results_this_outcome_df) <- c("Math GE", "English GE", "High GE")
  results <- rbind(results, results_this_outcome_df)
}

kable(results, format = "latex", booktabs = T, linesep = "")


kable(results, format = "latex", booktabs = T, align = c("rccc")) %>%
  save_kable("../Output/results.tex")

