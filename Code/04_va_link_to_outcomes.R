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
library(kableExtra)
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
  )

### 4. Run Analysis
outcomes <- c("dFreshOnTrack", "freshCoreGPA", "diff_grade_level", "pFreshAttendance",
              "nAPCourses4yr", "nHonorsCourses4yr",
              "highACT_EQUIV", "gradCumCoreGPA", "dEarnRegDip4yr",
              "dImm2yr", "dImm4yr", "dEarn2in3", "dEarn4in4")
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
subjects <- c("Math", "English")

top_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model2_fe, .90)
top_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model2_fe, .90)
low_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model2_fe, .05)
low_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model2_fe, .05)

results <- data.frame()
for(outcome in outcomes) {
  results_this_outcome <- c()
    
  ### Indep. Var 1 - Grade Effect (in each subject)
  ### Q: Am I doing this demeaning right?
  ### Q: Clustering at the class level?
  ### Q: Which Standard Errors/Clustering to Use
  for(current_sub in subjects) {
    va_scores_sub <- va_scores[va_scores$subject == current_sub,] 
    outcome_scores <- dresid(analytic_dataset, outcome, controls, current_sub)
    weights <- analytic_dataset %>% group_by(TID) %>% summarize(n = n())
    temp_data <- left_join(outcome_scores, va_scores_sub, by = "TID") %>%
      left_join(weights, by = "TID")
    reg1 <- lm(fixed_effects ~ va_model2_fe, data = temp_data, weights = n)
    test <- coeftest(reg1, plm::vcovHC(reg1, type = "HC3", cluster = c("funit")))
    results_this_outcome <- c(results_this_outcome, 
                              paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                              " [", round(test[2, 4], 3), "]"))
  }
  
  ### Test This:
  # for(current_sub in subjects) {
  #   temp_data <- filter(analytic_dataset, subject == current_sub) 
  #   outcome_resid <- dresid_2(temp_data, outcome, controls, current_sub)
  #   reg2 <- lm(fixed_effects ~ va_model2_fe, data = outcome_resid)
  #   temp_data <- innerjoin
  # }
  
  ### Indep. Var 2 _ Having a particularly high/low value add
  ### Generate variable indicating if a student had a teacher in the top 5% of VA for that subject
  ### TODO: Add descriptives on this variable
  ### TODO: Check, Do we pool for residualizing outcomes?
  for(current_sub in subjects) {
    va_scores_sub <- va_scores[va_scores$subject == current_sub,] 
    outcome_scores <- dresid(analytic_dataset, outcome, controls, current_sub)
    weights <- analytic_dataset %>% group_by(TID) %>% summarize(n = n())
    temp_data <- left_join(outcome_scores, va_scores_sub, by = "TID") %>%
      left_join(weights, by = "TID") %>%
      mutate(
        high_va = ifelse(current_sub == "Math" & (va_model2_fe >= top_math_cutoff), 1,
                         ifelse(current_sub == "English" & (va_model2_fe >= top_eng_cutoff), 1, 0))
      )
    reg2 <- lm(fixed_effects ~ high_va, data = temp_data, weights = n)
    test <- coeftest(reg2, plm::vcovHC(reg2, type = "HC3", cluster = c("funit")))
    results_this_outcome <- c(results_this_outcome, 
                              paste0(round(test[2,1], 3), " (", round(test[2,2], 3), ")",
                                     " [", round(test[2, 4], 3), "]"))
  }
  
  results_this_outcome_df <- data.frame(t(results_this_outcome), row.names = outcome)
  colnames(results_this_outcome_df) <- c("Math GE", "English GE", "High GE Math", "High GE English")
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
      linesep = c("", "", "", "", "", "\\addlinespace", "", "", "", "", "\\addlinespace",
                  "", "", "", "")) %>%
  save_kable("../Output/results.tex")

