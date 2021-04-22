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
library(gridExtra)
source("utils.R")

### 2. Load Analytic Dataset and Set Time Period for Analysis
analytic_dataset <- read_rds("/home/roymckenzie/Thesis/Output/analytic_dataset.rds")
first_year <- 2013
last_year <- 2013
year_list <- seq(first_year, last_year, 1)

### 3. Estimate the Value Added - Separate by Subject and Year
subjects <- c("Math", "English")
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
controls <- paste(controls, collapse = " + ")

#### 3a. Standardize Grade Difference
analytic_dataset <- analytic_dataset %>%
  group_by(subject) %>%
  mutate(
    grade_difference_std = (grade_difference-mean(grade_difference))/sd(grade_difference)
  )

for(yr in year_list) {
  for(current_sub in subjects) {
    temp_analytic_data <- filter(analytic_dataset, subject == current_sub & FRESH_COHORT_YEAR == yr)
    temp_analytic_data <- mutate(temp_analytic_data, 
                                 grade_difference_demean = grade_difference - mean(grade_difference))
    
    ### Model 1 - Simple OLS, Teacher Fixed Effects, No Controls.
    va_model1_lm <- lm(paste0("grade_difference ~ ", " + factor(TID) - 1"), 
                       data = temp_analytic_data)
    va_model1_fe <- va_model1_lm$coefficients 
    va_model1_fe <- data.frame(TID = names(va_model1_fe), va_model1_fe = va_model1_fe, row.names = NULL) %>%
      mutate(TID = as.numeric(str_replace(as.character(TID), fixed("factor(TID)"), ""))) %>%
      filter(!is.na(TID)) %>%
      mutate(va_model1_fe = va_model1_fe - mean(va_model1_fe)) %>%
      select(TID, va_model1_fe)

    ### Model 2 - Simple OLS, Teacher Fixed Effects - Taken From Original
    va_model2_lm <- plm(paste0("grade_difference ~ ", controls), 
                     data = temp_analytic_data,
                     index = c("TID", "SID"),
                     model = "within"
    )
    va_model2_fe <- fixef(va_model2_lm, type = "dmean")
    va_model2_fe <- data.frame(TID = names(va_model2_fe), va_model2_fe = va_model2_fe, row.names = NULL) %>%
      mutate(TID = as.numeric(as.character(TID))) %>%
      mutate(va_model2_fe = va_model2_fe - mean(va_model2_fe)) %>%
      select(TID, va_model2_fe)
    va_model2_resid <- cbind(va_model2_resid = as.vector(va_model2_lm$residuals),
                             attr(va_model2_lm$residuals, "index")) %>%
      mutate(
        TID = as.integer(as.character(TID)),
        SID = as.integer(as.character(SID))
      )
    
    ### Model 3 - Fixed effects repredicting residuals? ASK PETER
    va_model3_data <- temp_analytic_data %>%
      left_join(
        va_model2_resid, by = c("TID", "SID")
      ) %>%
      left_join(
        va_model2_fe, by = "TID"
      ) %>%
      mutate(
        grade_diff_resid = va_model2_resid + va_model2_fe
      )
    va_model3_lm <- lm(grade_diff_resid ~ factor(TID) - 1, data = va_model3_data)
    va_model3_fe <- va_model3_lm$coefficients
    va_model3_fe <- data.frame(TID = names(va_model3_fe), va_model3_fe = va_model3_fe, row.names = NULL) %>%
      mutate(TID = as.numeric(str_replace(as.character(TID), fixed("factor(TID)"), ""))) %>%
      mutate(va_model3_fe = va_model3_fe - mean(va_model3_fe)) %>%
      select(TID, va_model3_fe) 
    
    ### Model 4 - Conditional Bayes - TODO
    ### Model 5 - Conditional Bayes, accounting for drift? - TODO
    
    ### Merge Models
    temp <- full_join(va_model1_fe, va_model2_fe, by = "TID") %>%
      full_join(va_model3_fe, by = "TID") %>%
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

test <- dresid(analytic_dataset, "grade_difference", controls, c("Math", "English")) %>%
  left_join(va_output, by = "TID")

g1 <- ggplot(data = va_output[va_output$subject == "Math",]) + 
  geom_density(aes(x = va_model1_fe, color = "Model 1")) + 
  geom_density(aes(x = va_model2_fe, color = "Model 2")) + 
  geom_density(aes(x = va_model3_fe, color = "Model 3")) + 
  xlab("Grade Effect") + 
  labs(title = "Math")
g2 <- ggplot(data = va_output[va_output$subject == "English",]) + 
  geom_density(aes(x = va_model1_fe, color = "Model 1")) + 
  geom_density(aes(x = va_model2_fe, color = "Model 2")) + 
  geom_density(aes(x = va_model3_fe, color = "Model 3")) + 
  xlab("Grade Effect") + 
  labs(title = "English")

g3 <- arrangeGrob(g1, g2, nrow = 2)
ggsave("../Output/Grade_Effect_Density.jpeg", g3)

write_rds(va_output, "../Output/va_output.rds")
