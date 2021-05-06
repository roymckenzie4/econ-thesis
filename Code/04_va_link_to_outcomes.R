############ ---------------------------
##
## Script name: 04_va_link_to_outcomes.R
##
## Purpose of script: Estimate the relationship between VA models
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
library(kableExtra)
library(stringr)
library(sandwich)
source("utils.R")


first_year <- 2011
last_year <- 2014
year_list <- seq(first_year, last_year, 1)

### 2. Load Data
analytic_dataset <- read_rds("../Output/analytic_dataset.rds") %>%
  filter(
    subject == "Math" | subject == "English"
  )
analytic_cohort <- read_rds("../Output/analytic_cohort.rds") %>%
  filter(SID %in% analytic_dataset$SID)
va_scores <- read_rds("../Output/va_output.rds")

### 3. Merge Selected VA Score with Analytic Cohort
gclasses_outcomes <- read.csv("/home/projects/To_and_Through/Projects/MakeGraduatingClasses/Output/outcomes_210105.csv")
gclasses_outcomes_clean <- gclasses_outcomes %>%
  select(
    SID, dImm2yr, dImm4yr, dEarn2in3, dEarn4in4
  )

analytic_dataset <- left_join(analytic_dataset, va_scores, by = c("TID", "subject", "FRESH_COHORT_YEAR")) %>%
  left_join(gclasses_outcomes_clean, by = "SID") %>%
  mutate(
    diff_grade_level = ifelse(FRESH_SPRING_LEVEL == "R" & (SOPH_FALL_LEVEL == "H" | SOPH_FALL_LEVEL == "A"),
                              1, ifelse((FRESH_SPRING_LEVEL == "A" | FRESH_SPRING_LEVEL == "H") & SOPH_FALL_LEVEL == "H",
                                        -1, 0))
  ) %>%
  group_by(SID) %>%
  mutate(
    n_ap_after_fresh = nAPCourses4yr - sum(FRESH_SPRING_LEVEL == "A"),
    n_honors_after_fresh = nHonorsCourses4yr - sum(FRESH_SPRING_LEVEL == "H")
  ) 

### 4. Run Analysis
outcomes <- c("dFreshOnTrack", "freshCoreGPA", "diff_grade_level", "pFreshAttendance",
              "nAPCourses4yr", "n_honors_after_fresh",
              "highACT_EQUIV", "gradCumCoreGPA", "dEarnRegDip4yr",
              "dImm2yr", "dImm4yr", "dEarn2in3", "dEarn4in4")
controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend", 
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped", "PBPOV",
              "class_MATH_Z", "class_READ_Z", "class_age", "class_rnoAttend",
              "class_n_infractions_grade_8", "class_rnoCoreGpa", "class_size",
              "I(MATH_Z^2)", "I(READ_Z^2)", "I(age^2)", "I(rnoAttend^2)",
              "I(n_infractions_grade_8^2)", "I(rnoCoreGpa^2)",
              "I(MATH_Z^3)", "I(READ_Z^3)", "I(age^3)", "I(rnoAttend^3)",
              "I(n_infractions_grade_8^3)", "I(rnoCoreGpa^3)",
              "I(PBPOV^2)", "I(PBPOV^3)",
              "I(class_MATH_Z^2)", "I(class_READ_Z^2)", "I(class_age^2)", "I(class_rnoAttend^2)",
              "I(class_n_infractions_grade_8^2)", "I(class_rnoCoreGpa^2)",
              "I(class_MATH_Z^3)", "I(class_READ_Z^3)", "I(class_age^3)", "I(class_rnoAttend^3)",
              "I(class_n_infractions_grade_8^3)", "I(class_rnoCoreGpa^3)",
              "I(class_size^2)", "I(class_size^3)", 
              "school_MATH_Z", "school_READ_Z", "school_age", "school_rnoAttend",
              "school_n_infractions_grade_8", "school_rnoCoreGpa", "school_size",
              "I(school_MATH_Z^2)", "I(school_READ_Z^2)", "I(school_age^2)", "I(school_rnoAttend^2)",
              "I(school_n_infractions_grade_8^2)", "I(school_rnoCoreGpa^2)",
              "I(school_MATH_Z^3)", "I(school_READ_Z^3)", "I(school_age^3)", "I(school_rnoAttend^3)",
              "I(school_n_infractions_grade_8^3)", "I(school_rnoCoreGpa^3)",
              "I(school_size^2)", "I(school_size^3)",
              "class_pbpov", "school_pbpov", "I(class_pbpov^2)", "I(class_pbpov^3)",
              "I(school_pbpov^2)", "I(school_pbpov^3)")
controls <- paste(controls, collapse = " + ")
subjects <- c("Math", "English")

top_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model4_re, .90)
top_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model4_re, .90)
low_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model4_re, .05)
low_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model4_re, .05)

results <- data.frame()
for(outcome in outcomes) {
  results_this_outcome <- c()
    
  ### Indep. Var 1 - Grade Effect (in each subject)
  ### Q: Am I doing this demeaning right?
  ### Q: Clustering at the class level?
  ### Q: Which Standard Errors/Clustering to Use
  for(current_sub in subjects) {
    va_scores_sub <- va_scores[va_scores$subject == current_sub,] 
    outcome_scores <- dresid(analytic_dataset, outcome, controls, current_sub, year_list)
    weights <- analytic_dataset %>% group_by(TID, FRESH_COHORT_YEAR) %>% summarize(n = n())
    temp_data <- left_join(outcome_scores, va_scores_sub, by = c("TID", "FRESH_COHORT_YEAR")) %>%
      left_join(weights, by = c("TID", "FRESH_COHORT_YEAR"))
    reg1 <- lm(re ~ va_model4_re + factor(FRESH_COHORT_YEAR), data = temp_data, weights = n)
    test <- coeftest(reg1, sandwich::vcovHC(reg1, type = "const"))
    results_this_outcome <- c(results_this_outcome, 
                              paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                              " [", round(test[2, 4], 3), "]"))
    tem_data <- temp_data %>% 
      mutate(
        high_va = ifelse(current_sub == "Math" & (va_model4_re >= top_math_cutoff), 1,
                       ifelse(current_sub == "English" & (va_model4_re >= top_eng_cutoff), 1, 0))
        )
    reg2 <- lm(re ~ high_va + factor(FRESH_COHORT_YEAR), data = temp_data, weights = n)
    test <- coeftest(reg2, sandwich::vcovHC(reg2, type = "const"))
    results_this_outcome <- c(results_this_outcome, 
                              paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                                     " [", round(test[2, 4], 3), "]"))
  }
  
  ### Test This:
  # for(current_sub in subjects) {
  #   temp_data <- filter(analytic_dataset, subject == current_sub) 
  #   outcome_resid <- dresid_2(temp_data, outcome, controls, current_sub)
  #   reg2 <- lm(fixed_effects ~ va_model4_re, data = outcome_resid)
  #   temp_data <- innerjoin
  # }
  results_this_outcome_df <- data.frame(t(results_this_outcome), row.names = outcome)
  colnames(results_this_outcome_df) <- c("Math GE", "HighGE Math", "English GE", "High GE English")
  results <- rbind(results, results_this_outcome_df)
}

row.names(results) <- c("Freshman on Track", "Freshman Core GPA", "Difference in Class Level Freshman to Sophmore Year",
                        "Freshman Year Attendance (Fraction)",
                        "N. AP Courses Over 4 Years", "N. Honors Courses Over 4 Years",
                        "ACT Score", "Graduating Core GPA", "Earned a Diploma Within 4 Years",
                        "Enrolled Immediately in a 2 Year College", "Enrolled Immediately in a 4 Year College", 
                        "Earned a 2 Year Degree w/in 3 Years", "Earned a 4 Year Degree w/in 6 Years")

kable(results, format = "rst", booktabs = T, linesep = "")


kable(results, caption = "Impact of Teacher Grade Effect on a Variety of Outcomes\\label{tbl:results}", 
      format = "latex", booktabs = T, align = c("ccc"), midrule = "\\midrule", 
      linesep = c("", "", "", "\\addlinespace", "", "", "", "", "\\addlinespace",
                  "", "", "", "")) %>%
  save_kable("../Output/results.tex")

