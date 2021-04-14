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
library(janitor)
library(stringr)

### 2. Load Analytic Dataset and Set Time Period for Analysis
load("/home/roymckenzie/Thesis/Output/analytic_dataset.Rdata")
first_year <- 2013
last_year <- 2013
year_list <- seq(first_year, last_year, 1)

### 3. Estimate the Value Added - Separate by Subject and Year
subjects <- c("Math", "English", "Science", "Social Studies")

for(yr in year_list) {
  for(current_sub in subjects) {
    temp_analytic_data <- filter(analytic_dataset, subject == current_sub & FRESH_COHORT_YEAR == yr)
    ### Model 1 - Simple OLS, Teacher Fixed Effects, No Controls
    model1 <- lm(grade_difference ~ factor(TID) - 1, 
                  data = temp_analytic_data)
    va_model1 <- model1$coefficients
    va_model1 <- data.frame(TID = names(test), value = test, row.names = NULL) %>%
      mutate(TID = as.numeric(str_replace(as.character(TID), fixed("factor(TID)"), ""))) %>%
      select(TID, va_model1 = value)
    ### Model 2 - Simple OLS, Teacher Fixed Effects, Yes Controls
    model2 <- lm(grade_difference ~ MATH_Z + READ_Z + 
                   cRace + cGender + age + 
                   rnoAttend + n_infractions_grade_8)
  }
}
