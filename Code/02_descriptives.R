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
library(scales)
library(reshape2)
library(haven)
library(dplyr)
library(janitor)
library(kableExtra)
library(stringr)
library(stargazer)
library(xtable)
library(compareGroups)
library(scales)
library(reporttools)
library(ggplot2)
library(gtsummary)
library(gt)

theme_set(theme_minimal())



### 2. Load Data to Compare
analytic_cohort <- read_rds("../Output/analytic_cohort.rds") %>%
  mutate(
    analytic = 1
  ) %>%
  filter(
    freshCohort >= 2011 & freshCohort <= 2013
  )
raw_cohort <- read_rds("../Output/raw_cohort.rds") %>%
  mutate(
    analytic = 0
  )
analytic_dataset <- read_rds("../Output/analytic_dataset.rds") %>%
  filter(
    FRESH_COHORT_YEAR >= 2011 & FRESH_COHORT_YEAR <= 2013
  )

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
    freshCohort,
    age
  ) %>%
  mutate(
    PBPOV = 100*PBPOV,
    age = as.numeric(as.character(age))
  ) 

levels(temp$cRace)[levels(temp$cRace) == "Unknown Race/Ethnicity"] <- "Other"

sink("../Output/table_demographics.tex", type = c("output"))
temp_table <- tbl_summary(temp, sort = list(everything() ~ "frequency"),
            statistic = list(all_continuous() ~ "{mean} ({sd})"),
            digits = list(all_continuous() ~ 2),type = c("age" ~ "continuous"),
            label = c("age" ~ "Age at Start of Freshmen Year",
                      "freshCohort" ~ "Fall of Freshmen Year"), 
            missing_text = "Missing",
            by = freshCohort) %>% 
  modify_caption("Demographics by Freshman Cohort") %>%
  as_hux_table()

huxtable::number_format(temp_table)[1, ] <- "%.0f"
huxtable::label(temp_table) <- "tab:table_demographics"
huxtable::latex_float(temp_table) <- "h!"
huxtable::print_latex(temp_table) 
sink()

### 4b. Prior Performance Descriptives
sink("../Output/table_eigth_grade.tex")
temp_table <- tbl_summary(
  analytic_cohort, 
  include = c("MATH_Z", "READ_Z","rnoCoreGpa", "rnoAttend", "n_infractions_grade_8", "freshCohort"),
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  digits = list(all_continuous() ~ 4), 
  label = c("MATH_Z" ~ "8th Grade Math Score (Standardized)", "READ_Z" ~ "8th Grade Reading Score (Standardized)",
            "rnoAttend" ~ "8th Grade Attendance (Fraction of Days Attended)",
            "rnoCoreGpa" ~ "8th Grade Core GPA",
            "n_infractions_grade_8" ~ "8th Grade Discipline (no. of infractions)",
            "freshCohort" ~ "Fall of Freshman Year"), 
  by = freshCohort, 
  missing_text = "Missing"
) %>% 
modify_caption("8th Grade Outcomes by Cohorts") %>%
  modify_header(label = "**Outcome**") %>%
  as_hux_table() 

huxtable::number_format(temp_table)[1, ] <- "%.0f"
huxtable::label(temp_table) <- "tab:table_eigth_grade"
huxtable::latex_float(temp_table) <- "h!"
huxtable::print_latex(temp_table) 

sink()

### 4c. Class Data
temp <- analytic_dataset %>%
  filter(subject == "Math" | subject == "English") %>%
  group_by(FRESH_COHORT_YEAR, subject) %>%
  summarize(
    `Number of Students` = length(unique(SID)),
    `Number of Teachers` = length(unique(TID)),
    `Number of Schools` = length(unique(funit)),
    `Mean Freshman Grade` = paste0(format(mean(FRESH_SPRING_GPA), digits = 2, nsmall = 2), 
                                   " (", format(sd(FRESH_SPRING_GPA), digits = 2, nsmall = 2), ")"),
    `Mean Sophmore Grade` = paste0(format(mean(SOPH_FALL_GPA), digits = 2, nsmall = 2), 
                                   " (", format(sd(SOPH_FALL_GPA), digits = 2, nsmall = 2), ")"),
    `Mean Difference` = paste0(format(round(mean(FRESH_SPRING_GPA), 2) - round(mean(SOPH_FALL_GPA), 2), digits = 2, nsmall =  2),
                               " (", format(sd(FRESH_SPRING_GPA-SOPH_FALL_GPA), digits = 2, nsmall = 2), ")")
  ) %>% t() %>%
  row_to_names(row_number = 2)
  
kable(temp, format = "latex", caption = "Grade Variables by Freshman Cohort, Subject\\label{tab:table_freq}", 
      label = "table_freq", booktabs = T, linesep = c("", "", "\\addlinespace", "", "", "", ""),
      escape = F, align = c("cc"), midrule = "\\midrule") %>%
  add_header_above(c(" " = 1, "2011" = 2, "2012" = 2, "2013" = 2)) %>% 
  save_kable("../Output/table_freq.tex")

### 4d. Descriptives on Grades
### Table 1 - Average Grade in Each Class Over Time 
temp <- analytic_dataset %>%
  group_by(FRESH_COHORT_YEAR, subject) %>%
  summarize(
    mean_fall_gpa = format(mean(SOPH_FALL_GPA), digits = 2, nsmall = 2),
    sd_fall_gpa = format(sd(SOPH_FALL_GPA), digits = 2, nsmall = 2),
    mean_spring_gpa = format(mean(FRESH_SPRING_GPA), digits = 2, nsmall = 2),
    sd_spring_gpa = format(sd(FRESH_SPRING_GPA), digits = 2, nsmall = 2),
    mean_diff = format(mean(grade_difference), digits = 2, nsmall = 2),
    sd_diff = format(sd(grade_difference), digits = 2, nsmall = 2)
  ) %>%
  mutate(
    mean_spring_gpa = paste0(mean_spring_gpa, " (", sd_spring_gpa, ")"),
    mean_fall_gpa = paste0(mean_fall_gpa, " (", sd_fall_gpa, ")"),
    mean_diff = paste0(mean_diff, " (", sd_diff, ")")
  ) %>%
  select(-starts_with("sd")) %>%
  ungroup()

kable(temp, format = "latex", digits = 2, booktabs = T,
      col.names = c("", "", "Freshman Spring", "Sophmore Fall", "Difference"),
      caption = "Mean Grade (Weighted) Over Time by Cohort, Subject",
      align = c("l", "l", "c", "c", "c"), 
      label = "table_grade_desc") %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, latex_hline = "major") %>%
  save_kable("../Output/table_grade_desc.tex")

### Table 2 - Correlation Between Fall and Spring Grades (Math)
temp <- analytic_dataset %>%
  filter(subject == "Math") %>%
  select(
    FRESH_SPRING_FMK, 
    SOPH_FALL_FMK
  ) %>%
  table() %>%
  prop.table(margin = 1)%>%
  melt() 

g1 <- ggplot(temp, aes(FRESH_SPRING_FMK, SOPH_FALL_FMK)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = percent(value, accuracy = 1))) +
  scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue")) + 
  coord_flip() +
  xlab("Grade Freshman Spring") + 
  ylab("Grade Sophmore Fall") + 
  scale_y_discrete(position = "right", limits = rev(levels(temp$SOPH_FALL_FMK))) +
  theme(legend.position = "none") 
ggsave("../Output/grade_comp_math.png", plot = g1)

                       
temp <- analytic_dataset %>%
  filter(subject == "English") %>%
  select(
    FRESH_SPRING_FMK, 
    SOPH_FALL_FMK
  ) %>%
  table() %>%
  prop.table(margin = 1)%>%
  melt() 

g2 <- ggplot(temp, aes(FRESH_SPRING_FMK, SOPH_FALL_FMK)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = percent(value, accuracy = 1))) +
  scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue")) + 
  coord_flip() +
  xlab("Grade Freshman Spring") + 
  ylab("Grade Sophmore Fall") + 
  scale_y_discrete(position = "right", limits = rev(levels(temp$SOPH_FALL_FMK))) +
  theme(legend.position = "none")
ggsave("../Output/grade_comp_eng.png", plot = g2)
