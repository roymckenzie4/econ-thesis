############ ---------------------------
##
## Script name: 01_data_cleaning.R
##
## Purpose of script: Set up initial data cleaning for thesis project.
##
## Author: Roy McKenzie
##
## Date Created: 3-29-2021
##
## ---------------------------

## 1. Load Packages and helpful code base
library(tidyverse)
library(haven)
library(dplyr)
library(janitor)

## 2. Set up Parameters for analysis
first_year <- 2013
last_year <- 2013

## 3. Read Grade Data

# First, get our freshman cohort. This indicates our base cohort under consideration
fclasses <- read.csv("/home/projects/To_and_Through/DATA/newFcohortsOutput/allFcohorts2020_2_9_21.csv") %>%
  filter(freshCohort >= first_year & freshCohort <= last_year)

# Next, load in the corresponding grade data for both the freshman and sophmore years of each 
# cohort considered. 
read_grades_per_cohort <- function(first_year, last_year, fclasses) {
  
  results <- list()
  
  for(year in first_year:(last_year + 1)) {
    short_year <- year %% 1000 + 1
    # Load in the data
    fall_grade_data <- read_sas(paste0("/mnt/data/grade_files/single_line/gradesched", 
                                       short_year, 
                                       "1.sas7bdat")) %>%
      clean_names(case = "all_caps")
    
    spring_grade_data <- read_sas(paste0("/mnt/data/grade_files/single_line/gradesched", 
                                         short_year, 
                                         "2.sas7bdat")) %>%
      clean_names(case = "all_caps")
    
    grade_data <- rbind(fall_grade_data, spring_grade_data) %>%
      mutate_if(is.character, str_trim) %>%
      mutate(
        TID = ifelse(TID == 999999999, NA, TID)
      ) %>%
      filter(CN3 >= 100 & CN3 < 500) %>% # Get only core subjects 
      filter(SID %in% fclasses$SID) %>%
      filter(CRATTX8 > 0) %>%
      distinct(SID, CN3, SEMESTER, .keep_all = TRUE) %>%
      filter(!is.na(TID)) %>%
      mutate(
        subject = ifelse(CN3 >= 100, "English", NA),
        subject = ifelse(CN3 >= 200, "Social Studies", subject),
        subject = ifelse(CN3 >= 300, "Science", subject), 
        subject = ifelse(CN3 >= 400, "Math", subject)
      ) %>%
      mutate(
        GPA = factor(FMK, levels = c("/", "P", "A", "B", "C", "D", "F"), labels = c(NA, NA, 4, 3, 2, 1, 0)),
        GPA = as.numeric(as.character(GPA)),
        GPA = ifelse(LEVEL == "A", GPA + 2, GPA),
        GPA = ifelse(LEVEL == "H", GPA + 1, GPA)
      ) %>%
      group_by(SID, CN3, subject, TID) %>%
      summarize(
        GPA = mean(GPA)
      ) %>%
      mutate(
        YEAR = year
      )
    
    results[[year - first_year + 1]] <- grade_data
  }
}

test <- left_join(results[[1]], results[[2]][c("SID", "subject")], by = c("SID", "subject"))

