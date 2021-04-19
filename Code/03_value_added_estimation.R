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
library(rlist)
library(plm)
library(stringr)

### 2. Load Analytic Dataset and Set Time Period for Analysis
analytic_dataset <- read_rds("/home/roymckenzie/Thesis/Output/analytic_dataset.rds")
first_year <- 2013
last_year <- 2013
year_list <- seq(first_year, last_year, 1)

### 3. Estimate the Value Added - Separate by Subject and Year
subjects <- c("Math", "English")
controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend", 
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped")
controls <- paste(controls, collapse = " + ")

for(yr in year_list) {
  for(current_sub in subjects) {
    temp_analytic_data <- filter(analytic_dataset, subject == current_sub & FRESH_COHORT_YEAR == yr)
    temp_analytic_data <- mutate(temp_analytic_data, 
                                 grade_difference_demean = grade_difference - mean(grade_difference), 
                                 test = 1)
    
    ### Model 1 - Simple OLS, Teacher Fixed Effects, No Controls.
    va_model1 <- lm(grade_difference ~ factor(TID) - 1, 
                    data = temp_analytic_data)
    va_model1 <- va_model1$coefficients
    va_model1 <- data.frame(TID = names(va_model1), value = va_model1, row.names = NULL) %>%
      mutate(TID = as.numeric(str_replace(as.character(TID), fixed("factor(TID)"), "")),
             va_model1 = (va_model1 - mean(va_model1))/sd(va_model1)) %>%
      select(TID, va_model1 = value) 
    
    ### Model 2 - Simple OLS, Teacher Fixed Effects - Taken From Original
    va_model2 <- plm(paste0("grade_difference ~ ", controls), 
                     data = temp_analytic_data,
                     index = c("TID"),
                     model = "within"
    )
    va_model2_residuals <- data.frame(resid_init = va_model2$residuals)
    va_model2 <- fixef(va_model2, type = "dmean")
    va_model2 <- data.frame(TID = names(va_model2), va_model2 = va_model2, row.names = NULL) %>%
      mutate(TID = as.numeric(as.character(TID)),
             va_model2 = (va_model2 - mean(va_model2))/sd(va_model2)) %>%
      select(TID, va_model2)
    
    ### Model 3 - Fixed effects repredicting residuals? ASK PETER
    va_model3 <- left_join(temp_analytic_data, va_model2, by = "TID") %>%
      cbind(va_model2_residuals)
    va_model3 <- va_model3 %>%
      mutate(
        correct_outcome = va_model2 + resid_init
      )
    va_model3 <- lm(correct_outcome ~ factor(TID) - 1, data = va_model3)
    va_model3 <- va_model3$coefficients
    va_model3 <- data.frame(TID = names(va_model3), value = va_model3, row.names = NULL) %>%
      mutate(TID = as.numeric(str_replace(as.character(TID), fixed("factor(TID)"), "")),
             va_model3 = (va_model3 - mean(va_model3)) / sd(va_model3)) %>%
      select(TID, va_model3 = value) 
    
    ### Model 4 - Conditional Bayes - TODO
    ### Model 5 - Conditional Bayes, accounting for drift? - TODO
    
    ### Merge Models
    temp <- left_join(va_model1, va_model2, by = "TID") %>%
      left_join(va_model3, by = "TID") %>%
      mutate(
        subject = current_sub, 
        FRESH_COHORT_YEAR = yr
      )
    
    assign(paste0("va_measures_", current_sub), temp) 
  }
  temp <- list.rbind(lapply(paste0("va_measures_", subjects), get))
  assign(paste0("va_measures_", yr), temp)
}

va_output <- list.rbind(lapply(paste0("va_measures_", year_list), get))
write_rds(va_output, "../Output/va_output.rds")
