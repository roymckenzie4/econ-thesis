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
library(stargazer)
library(dplyr)
library(lmtest)
library(janitor)
library(kableExtra)
library(stringr)
library(sandwich)
library(plm)
source("utils.R")


first_year <- 2011
last_year <- 2013
year_list <- seq(first_year, last_year, 1)

### 2. Load Data
analytic_dataset <- read_rds("../Output/analytic_dataset.rds") %>%
  filter(
    subject == "Math" | subject == "English"
  )
analytic_cohort <- read_rds("../Output/analytic_cohort.rds") %>%
  filter(SID %in% analytic_dataset$SID)
va_scores <- read_rds("../Output/va_output.rds") %>%
  filter(FRESH_COHORT_YEAR %in% year_list)

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
  ) %>%
  mutate(
    immEnr = dImm2yr + dImm4yr,
    on_time_graduation = dEarn2in3 + dEarn4in4
  )

### 4. Run Analysis
outcome_sets <- list(
  c("dFreshOnTrack", "freshCoreGPA", "pFreshAttendance"),
  c("nAPCourses4yr", "n_honors_after_fresh"), 
  c("highACT_EQUIV", "gradCumCoreGPA", "gradCumOverallGPA"),
  c("dEarnAnyDip4yr", "immEnr", "on_time_graduation")
)
outcome_names <- list(
  c("Freshman On Track", "Freshman Core GPA", "Freshman Attendance"),
  c("N. AP Courses After Freshman Year", "N. Honors Courses After Freshman Year"), 
  c("ACT/SAT Score (ACT Scale)", "Graduating Core GPA", "Graduating Overall GPA"),
  c("Graduate HS in 4 Years", "Immediately Enrolled in College", "On Time College Graduation")
)
outcome_set_names <- c("Freshman Outcomes", "Course Taking Patterns", "Academic Outcomes", "Long Term Outcomes")
file_names <- c("fresh_out", "course_out", "academic_out", "long_out")
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

top_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model4_re, .90, na.rm = TRUE)
top_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model4_re, .90, na.rm = TRUE)
low_math_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "Math",]$va_model4_re, .05, na.rm = TRUE)
low_eng_cutoff <- quantile(analytic_dataset[analytic_dataset$subject == "English",]$va_model4_re, .05, na.rm = TRUE)

# Q: Should we change weights to only include n students with outcome variable?
weights <- analytic_dataset %>% 
  group_by(TID, FRESH_COHORT_YEAR, subject) %>% 
  summarize(n = n()) %>%
  ungroup()

for(i in 1:4) {
  outcomes <- outcome_sets[[i]]
  means <- c("Mean (All Years)")
  models <- list()
  se <- list()
  
  for(outcome in outcomes) {
    outcome_scores <- dresid(analytic_dataset, outcome, controls, subjects, year_list)
    plot_data <- data.frame()

    ### Indep. Var 1 - Grade Effect (in each subject)
    ### Q: Am I doing this demeaning right?
    ### Q: Clustering at the class level?
    ### Q: Which Standard Errors/Clustering to Use
    for(current_sub in subjects) {
      means <- c(means, round(mean(analytic_dataset[analytic_dataset$subject == current_sub,][[outcome]], na.rm = TRUE), 3))
      va_scores_sub <- va_scores %>%
        filter(subject == current_sub) %>%
        mutate(
          va_model4_re_std = (va_model4_re - mean(va_model4_re)) / sd(va_model4_re)
        )
      
      temp_data <- left_join(va_scores_sub, outcome_scores, by = c("TID", "FRESH_COHORT_YEAR", "subject")) %>%
        left_join(weights, by = c("TID", "FRESH_COHORT_YEAR", "subject")) %>%
        rename(!!outcome := re)
      formula <- paste0(outcome, " ~ va_model4_re + factor(FRESH_COHORT_YEAR)")
      reg1 <- lm(formula, data = temp_data, weights = n)
      test <- coeftest(reg1, sandwich::vcovHC(reg1, type = "HC1"))[,2]
      models <- list.append(models, reg1)
      se <- list.append(se, test)

      analytic_dataset_temp  <- analytic_dataset %>% 
        ungroup() %>%
        inner_join(va_scores_sub, by = c("TID", "FRESH_COHORT_YEAR", "subject")) %>%
        inner_join(outcome_scores, by = c("TID", "FRESH_COHORT_YEAR", "subject")) %>%
        mutate(
          re = re + mean(!!as.name(outcome), na.rm = TRUE),
        )
      plot_data <- rbind(plot_data, analytic_dataset_temp)
    }
    
    plot <- ggplot() + 
      stat_summary_bin(data = plot_data,
                       fun = "mean", bins = 20, 
                       aes(x = va_model4_re_std, 
                           y = re, color = subject)) + 
      xlab("Teacher Grade Effect") + 
      ylab(outcome_names[[i]][which(outcomes == outcome)]) + 
      labs(color = "Subject")
    ggsave(paste0("../Output/", outcome, "_scatter.png"), plot)
  }
    
  stargazer(models, type = "latex", 
            title = paste0("Impact of Standardized Teacher Grade Effects on ", outcome_set_names[i]),
            column.labels = rep(c("Math", "English"), 10), omit = c("FRESH_COHORT_YEAR"), 
            keep = "va_model4_re",
            omit.labels = c("Year Dummies"), style = "aer", 
            covariate.labels = "Teacher Grade Effect", model.numbers = FALSE, dep.var.caption = "",
            keep.stat = c("n"),
            dep.var.labels = outcome_names[[i]],
            dep.var.labels.include = TRUE, se = se, notes.align = "r", 
            notes = c("This table displays the outcome of regressing residualized", 
                      "outcomes on teacher grade effects by subject and outcome,",
                      "allowing for year fixed effects. Heteroskedasticity robust",
                      "standard errors are used"),
            font.size = "scriptsize", add.lines = list(means), 
            label = paste0("tab:", file_names[i]),
            out = paste0("../Output/", file_names[i], ".tex"), 
            digits = 3, align = T)
}

