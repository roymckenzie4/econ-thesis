############ ---------------------------
##
## Script name: 02_descriptives.R
##
## Purpose of script: Create descriptive tables for thesis project. 
##
## Author: Roy McKenzie
##
## Date Created: 3-29-2021
##
## ---------------------------

### 1. Load Libraries and utility functions
library(tidyverse)
library(haven)
library(dplyr)
library(janitor)
library(stringr)
library(stargazer)
library(xtable)
library(compareGroups)
library(scales)
library(reporttools)
library(gtsummary)
library(gt)

### 2. Load Data to Compare
analytic_cohort <- read_rds("../Output/analytic_cohort.rds") %>%
  mutate(
    analytic = 1
  )
raw_cohort <- read_rds("../Output/raw_cohort.rds") %>%
  mutate(
    analytic = 0
  )
analytic_dataset <- read_rds("../Output/analytic_dataset.rds")

### 3. Comparing Analytic and Raw Cohort

# TODO

### 4. Descriptives of Analytic Cohort

### 4a. Demographic Descriptives
attr(analytic_cohort$cRace, "label") <- "Race"
attr(analytic_cohort$cGender, "label") <- "Gender"
attr(analytic_cohort$PBPOV, "label") <- "Census Block - % Families Below Poverty"
temp <- analytic_cohort %>%
  select(
    cRace, 
    cGender,
    PBPOV, 
    age
  ) %>%
  mutate(
    PBPOV = 100*PBPOV,
    age = as.numeric(as.character(age))
  )
tbl_summary(temp, sort = list(everything() ~ "frequency"),
            statistic = list(all_continuous() ~ "{mean} ({sd})"),
            digits = list(all_continuous() ~ 2),type = c("age" ~ "continuous"),
            label = c("age" ~ "Age at Start of Freshmen Year"), missing_text = "Missing") %>% 
  as_hux_table() %>% huxtable::quick_latex("")

### 4b. Prior Performance Descriptives
tbl_summary(
  analytic_cohort, include = c("MATH_Z", "READ_Z", "rnoAttend", "n_infractions_grade_8"),
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  digits = list(all_continuous() ~ 2), 
  label = c("MATH_Z" ~ "8th Grade Math Score (Standardized)", "READ_Z" ~ "8th Grade Reading Score (Standardized)",
            "rnoAttend" ~ "8th Grade Attendance (Fraction of Days Attended)",
            "n_infractions_grade_8" ~ "8th Grade Discipline (no. of infractions)"), 
  missing_text = "Missing"
) %>% as_hux_table() %>% huxtable::print_latex()

### 4c. Class Data
temp <- analytic_dataset %>%
  summarize(
    `Number of Students` = length(unique(SID)),
    `Number of Teachers` = length(unique(TID)),
    `Number of Schools` = length(unique(funit)),
    `Math Grades` = sum(subject == "Math"),
    `Reading Grades` = sum(subject == "English"),
    `Social Studies Grades` = sum(subject == "Social Studies"), 
    `Science Grades` = sum(subject == "Science")
  ) %>%
  t()
xtable(temp, caption = "Descriptives of Grade Data") %>%
  print.xtable(include.colnames = FALSE) 
