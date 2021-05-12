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
library(knitr)
library(stringr)
library(sandwich)
source("utils.R")

first_year <- 2010
last_year <- 2010
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
teacher_data_10 <- read_sas("/mnt/data/CPS_personnel/teacher/teachers1011.sas7bdat")
teacher_data_11 <- read_sas("/mnt/data/CPS_personnel/teacher/teachers1112.sas7bdat")

teacher_data <- rbind(teacher_data_10, teacher_data_11)

#### Test A. Balance Tests - Teacher on Teacher
first_cohort <- filter(analytic_dataset, FRESH_COHORT_YEAR == 2010)
fresh_teachers <- teacher_data_11 %>% filter(
  EMPLOYEE_ID %in% first_cohort$TID
)
soph_teachers <- teacher_data_11 %>% filter(
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

stargazer(list(test1, test2, test3), omit = c("GRADE_SCHLID"), 
          title = "Link Between Freshman and Sophmore Year Teachers",
          omit.labels = c("School Fixed Effects?"), style = "aer", 
          covariate.labels = c("Masters Degree", "Years of Service", "National Board Certification"),
          model.numbers = FALSE, dep.var.caption = "Sophmore Year",
          column.labels = "Sophmore Year", column.separate = c(3),
          type = "latex",
          keep = c("DEGREE_MASTERS", "YEARS_OF_SERVICE", "NATIONAL_BOARD_CERTIFIED"),
          keep.stat = c("n"),
          dep.var.labels = c("Masters Degree", "Years of Service", "National Board Certification"),
          dep.var.labels.include = TRUE, notes.align = "r", 
          font.size = "scriptsize",
          notes = c("Constructed using pairs of core call teachers linked across",
                    "Freshman and Sophmore year. Using 2010-2011 Freshan cohort."),
          label = paste0("tab:test1"),
          out = paste0("../Output/test1.tex"), 
          digits = 3, align = T)

### Seems to be consistently significant but small effects


### Test B. Relationship Between Teacher Qualities and Grade Effect

eleven_cohort <- filter(analytic_dataset, FRESH_COHORT_YEAR == 2011)
eleven_teachers <- teacher_data_11 %>% filter(
  EMPLOYEE_ID %in% eleven_cohort$TID
)

test_b_data <- eleven_cohort %>% 
  inner_join(eleven_teachers, by = c("TID" = "EMPLOYEE_ID")) %>%
  left_join(va_scores, by = c("TID", "subject", "FRESH_COHORT_YEAR")) %>%
  mutate(
    max_doc = ifelse(DEGREE_DOCTORATE == 1, 1, 0),
    max_mas = ifelse(DEGREE_MASTERS == 1 & max_doc == 0, 1, 0),
    max_bac = ifelse(DEGREE_BACHELORS == 1 & max_mas == 0, 1, 0),
    max_ass = ifelse(DEGREE_ASSOCIATES == 1 & max_bac == 0, 1, 0),
    max_edu = ifelse(max_doc, 4, 
                     ifelse(max_mas, 3, 
                            ifelse(max_bac, 2,
                                   ifelse(max_ass, 1, 0))))
  )

testb1 <- lm(va_model4_re ~ max_ass + max_bac + max_mas + max_doc + YEARS_OF_SERVICE +
               BILINGUAL_TEACH_CERTIFICATE + SPECED_TEACH_CERTIFICATE + 
               factor(GRADE_SCHLID),
             data = test_b_data)

test_stat_b1 <- sandwich::vcovHC(testb1, type = "HC1", cluster = "SID")[,2]

stargazer(list(testb1), omit = c("GRADE_SCHLID"), 
          title = "Link Between Teacher Characteristics and Grade Effect",
          omit.labels = c("School Fixed Effects?"), style = "aer", 
          covariate.labels = c("Max Degree - Associates Degree", "Max Degree - Bachelors",
                               "Max Degree - Masters", 
                               "Max Degree - Doctoral", "Years of Service", "Bilingual Certification",
                               "SPED Certification"),
          model.numbers = FALSE, dep.var.caption = "Sophmore Year",
          column.labels = "Sophmore Year", column.separate = c(3),
          type = "latex",
          keep = c("max_ass", "max_bac", "max_mas", "max_doc", "YEARS_OF_SERVICE",
                   "BILINGUAL_TEACH_CERTIFICATE", "SPECED_TEACH_CERTIFICATE"),
          keep.stat = c("n"),
          dep.var.labels = c("Grade Effect"),
          dep.var.labels.include = TRUE, notes.align = "r", 
          font.size = "scriptsize",
          notes = c("Constructed using the 2011-2012 Freshman cohort."),
          label = paste0("tab:test2"),
          out = paste0("../Output/test2.tex"), 
          se = list(test_stat_b1),
          digits = 3, align = T)


### Test C. Regressing Assigned Teacher Characteristics on Non-Included Student
### Features. 

test_c_data <- test_b_data %>%
  mutate(rnoAttend = 100 * rnoAttend)
student_controls <- c("MATH_Z", "READ_Z", "cRace", "cGender", "age", "rnoAttend", 
              "n_infractions_grade_8", "rnoCoreGpa", "dFreshSped", "PBPOV")
student_controls_academic <- c("MATH_Z", "READ_Z", "rnoCoreGpa")
student_controls_behavioral <- c("rnoAttend", "n_infractions_grade_8")
student_controls_demographic <- c("cRace", "cGender", "age", "dFreshSped", "PBPOV")
testc1 <- lm(paste0("max_edu ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_behavioral, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc2 <- lm(paste0("max_edu ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc3 <- lm(paste0("max_edu ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_behavioral, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc4 <- lm(paste0("max_edu ~ ", 
                    paste(student_controls_behavioral, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)

testc1_y <- lm(paste0("YEARS_OF_SERVICE ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_behavioral, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc2_y <- lm(paste0("YEARS_OF_SERVICE ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc3_y <- lm(paste0("YEARS_OF_SERVICE ~ ", 
                    paste(student_controls_academic, collapse = "+"), "+",
                    paste(student_controls_behavioral, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)
testc4_y <- lm(paste0("YEARS_OF_SERVICE ~ ", 
                    paste(student_controls_behavioral, collapse = "+"), "+",
                    paste(student_controls_demographic, collapse = "+"),
                    " + factor(GRADE_SCHLID)"),
             data = test_c_data)

stargazer(list(testc1, testc2, testc3, testc1_y, testc2_y, testc3_y), type = "latex",
          keep = c("MATH_Z", "READ_Z", "rnoCoreGpa"),
          model.numbers = FALSE,
          dep.var.labels = c("Max Level of Education", "Years of Service"),
          style = "aer",
          omit = c("GRADE_SCHLID", "rnoAttend", "cRace"), 
          omit.labels = c("School Fixed Effects?", 
                          "Behavioral Controls?", 
                          "Demographic Controls?"), 
          keep.stat = c("n"),
          font.size = "scriptsize",
          digits = 3,
          notes.align = "r",
          out = "../Output/test3_1.tex",
          label = "tab:test3_1", 
          align = T,
          covariate.labels = c("8th Grade Math", "8th Grade Reading", "8th Grade Core GPA"),
          notes = c("Education levels coded as 1 = Associates,",
                    "2 = Bachelors, etc. Eighth grade test scores",
                    "are standardized within year.",
                    "Created using 2011-12 Freshmen cohort."), 
          title = "Checking Balance of Academic Controls with Different Sets of Controls")

stargazer(list(testc4, testc2, testc3, testc4_y, testc2_y, testc3_y), 
          type = "latex",
          keep = student_controls_demographic,
          model.numbers = FALSE,
          dep.var.labels = c("Max Level of Education", "Years of Service"),
          style = "aer",
          omit = c("GRADE_SCHLID", "rnoAttend", "rnoCoreGpa"), 
          omit.labels = c("School Fixed Effects?", 
                          "Behavioral Controls?", 
                          "Academic Controls?"), 
          keep.stat = c("n"),
          font.size = "scriptsize",
          out = "../Output/test3_2.tex",
          label = "tab:test3_2",
          digits = 3,
          notes.align = "r",
          align = T,
          covariate.labels = c("Race - Black", "Race - Latino", "Race - Multiracial", "Race - Other", 
                               "Race - Unknown", "Race - White", "Gender - Male", "Age", "SPED", 
                               "Census Block Poverty Level"),
          notes = c("Education levels coded as 1 = Associates,",
                    "2 = Bachelors, etc. Base race category is", 
                    "Asian. Census block poverty measured by",
                    "percent of families below the poverty line.",
                    "Created using 2011-12 Freshman cohort."), 
          title = "Checking Balance of Demographic Controls with Different Sets of Controls")

stargazer(list(testc4, testc1, testc3, testc4_y, testc1_y, testc3_y), 
          type = "latex",
          keep = student_controls_behavioral,
          model.numbers = FALSE,
          dep.var.labels = c("Max Level of Education", "Years of Service"),
          style = "aer",
          omit = c("GRADE_SCHLID", "cRace", "rnoCoreGpa"), 
          omit.labels = c("School Fixed Effects?", 
                          "Demographic Controls?", 
                          "Academic Controls?"), 
          keep.stat = c("n"),
          font.size = "scriptsize",
          digits = 3,
          notes.align = "r",
          align = T,
          column.sep.width = "-6pt",
          out = "../Output/test3_3.tex",
          label = "tab:test3_3",
          covariate.labels = c("8th Grade Attendance (\\%)", "N. Infraction Grade 8"),
          notes = c("Education levels coded as 1 = Associates,",
                    "2 = Bachelors, etc. Created using 2011-12 Freshman cohort."), 
          title = "Checking Balance of Behavioral Controls with Different Sets of Controls")


### One More Test
test_d_data <- va_scores %>% 
  ungroup() %>%
  select(TID, subject, FRESH_COHORT_YEAR, va_model4_re) %>%
  pivot_wider(id_cols = c(TID, subject), names_from = c(FRESH_COHORT_YEAR),
              values_from = va_model4_re, names_prefix = "", values_fill = NA)

cor(test_d_data[,3:5], use = "pairwise") %>% 
  kable(format = "latex", booktabs = T, col.names = c("2011", "2012", "2013"), digits = 3, 
        caption = "Correlation of Estimated Grade Effects Across Years\\label{tab:autocorrelation}") %>%
  kable_styling() %>%
  add_footnote("\\textit{Note:} Constructed using pairwise complete estimates of grade effects for Freshman core course teachers from the 2011-12 to 2013-14 school years.", 
               "none", escape = FALSE) %>%
  column_spec(2:4, width = "2cm") %>%
  save_kable("../Output/autocorrelation.tex")
