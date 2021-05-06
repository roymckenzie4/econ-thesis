############ ---------------------------
##
## Script name: 05_tests.R
##
## Purpose of script: Run robustness checks.
##
## Author: Roy McKenzie
##
## Date Created: 05-04-2021
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

### 3. Loading Teacher Data
teacher_data_11 <- read_sas("/mnt/data/CPS_personnel/teacher/teachers1011.sas7bdat")
teacher_data_12 <- read_sas("/mnt/data/CPS_personnel/teacher/teachers1112.sas7bdat")

teacher_data <- rbind(teacher_data_11, teacher_data_12)

#### Test A. Balance Tests
first_cohort <- filter(analytic_dataset, FRESH_COHORT_YEAR == 2010)
fresh_teachers <- teacher_data_12 %>% filter(
  EMPLOYEE_ID %in% first_cohort$TID
)
soph_teachers <- teacher_data_12 %>% filter(
  EMPLOYEE_ID %in% first_cohort$SOPH_TID
)
teacher_pairs <- first_cohort %>% 
  group_by(GRADE_SCHLID, TID, SOPH_TID, subject) %>% 
  summarize(
    n_students_in_pair = n()
  ) %>%
  ungroup() %>%
  group_by(GRADE_SCHLID, TID) %>%
  mutate(
    n_students_with_t1 = sum(n_students_in_pair)
  ) %>%
  ungroup() %>% 
  mutate(
    prop_in_path = n_students_in_pair / n_students_with_t1 
  )

teacher_pairs <- teacher_pairs %>%
  left_join(
    fresh_teachers, by = c("TID" = "EMPLOYEE_ID")
  ) %>%
  right_join(
    soph_teachers, by = c("SOPH_TID" = "EMPLOYEE_ID"),
    suffix = c("", "_soph")
  )

test1 <- lm(DEGREE_MASTERS_soph ~ DEGREE_MASTERS + factor(GRADE_SCHLID), 
            data = teacher_pairs, weights = n_students_in_pair)
test2 <- lm(YEARS_OF_SERVICE_soph ~ YEARS_OF_SERVICE + factor(GRADE_SCHLID),
            data = teacher_pairs, weights = n_students_in_pair)
test3 <- lm(NATIONAL_BOARD_CERTIFIED_soph ~ NATIONAL_BOARD_CERTIFIED + factor(GRADE_SCHLID),
            data = teacher_pairs, weights = n_students_in_pair)

### Seems to be consistently significant but small effects

### Test B. Relationship Between Teacher Qualities and Grade Effect
test_b_data <- first_cohort %>% 
 inner_join(fresh_teachers, by = c("TID" = "EMPLOYEE_ID")) 

testb1 <- lm(va_model4_re ~ DEGREE_ASSOCIATES + DEGREE_BACHELORS + DEGREE_MASTERS + 
               DEGREE_DOCTORATE + YEARS_OF_SERVICE +
               BILINGUAL_TEACH_CERTIFICATE + SPECED_TEACH_CERTIFICATE + 
               factor(GRADE_SCHLID),
             data = test_b_data)

### Test C. Regressing Assigned Teacher Characteristics on Non-Included Student
### Features. 

test_c_data <- test_b_data 
student_controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend", 
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped", "PBPOV")
testc1 <- lm(paste0("DEGREE_MASTERS ~ ", 
                    paste(student_controls, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
                  data = test_c_data)

