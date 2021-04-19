############ ---------------------------
##
## Script name: utils.R
##
## Purpose of script: Hold utility functions for overall analysis. 
##
## Author: Roy McKenzie (Some functions from To&Through Team)
##
## Date Created: 4-10-2021
##
## ---------------------------

library(tidyverse)
library(haven)
library(tidyselect)
library(readxl)

#### Section 1 - Function to Assist in Reading in Grade Files
#### This function are based off those in the fil 00a_read_in_data.R from 
#### the To&Through MakeFreshmenCohorts code, and have been contributed
#### to greatly by the entire To&Through team. 


#' This function reads and merges in the grades files by cohort. 
#' N.B. Unlike the original version, this function ONLY work fors years AFTER 2007.
#' 
#' @param first_cohort First school year for which to read grades (FALL INDEXED)
#' @param last_cohort  Last school year for which to read grades (FALL INDEXED)
#' @return A dataframe containing all grades for the given year, cleaned. 
read_grades <- function(first_cohort, last_cohort) {
  
  # Create an empty dataframe to house the final dataset
  grades <- data.frame()
  
  # Loop through cohorts, reading grade data and combining
  for (yr in (first_cohort:last_cohort)) {
    # print(yr) 
    fall_without_prelim <- paste0('/mnt/data/grade_files/single_line/gradesched',substr(yr+1,3,4),'1.sas7bdat')
    fall_with_prelim <- paste0('/mnt/data/grade_files/single_line/grade_nosched',substr(yr+1,3,4),'1_prelim.sas7bdat')
    
    spring_without_prelim <- paste0('/mnt/data/grade_files/single_line/gradesched',substr(yr+1,3,4),'2.sas7bdat')
    spring_with_prelim <- paste0('/mnt/data/grade_files/single_line/grade_nosched',substr(yr+1,3,4),'2_prelim.sas7bdat')
    
    if (file.exists(fall_without_prelim)) {
      fall_file <- fall_without_prelim
    } else {
      fall_file <- fall_with_prelim
    }
    
    if (file.exists(spring_without_prelim)) {
      spring_file <- spring_without_prelim
    } else {
      spring_file <- spring_with_prelim
    }
    
    long_fall_grades <- read_sas(fall_file)
    long_spring_grades <- read_sas(spring_file)
    
    names(long_fall_grades) <- toupper(names(long_fall_grades))
    names(long_spring_grades) <- toupper(names(long_spring_grades))
    
    long_fall_grades <- long_fall_grades %>% 
      rename_all(.funs = funs(sub("FA[0-9][0-9]", "", names(long_fall_grades)))) %>%
      mutate(SCHLYR=yr,
             CN3=ifelse(SCHLYR %in% c(2008,2009),as.numeric(CN),as.numeric(CN3)),
             SID=ifelse(SCHLYR %in% c(2008,2009),as.numeric(STUID),as.numeric(SID)),
             LEVEL=ifelse(SCHLYR %in% c(2008,2009),LEV,LEVEL),
             TITLE=ifelse(SCHLYR %in% c(2008,2009),SUB,TITLE),
             CRATTX8=ifelse(SCHLYR %in% c(2008,2009),CRD,CRATTX8)) %>%
      select(SID, SCHLID, FMK,CRATTX8,LEVEL,CN3, CN4, CN, TITLE,
             SECTION, TID, TIDM1, TIDM2) %>%
      mutate(fall_semester = 1)
    
    long_spring_grades <- long_spring_grades %>% 
      rename_all(.funs = funs(sub("SP[0-9][0-9]", "", names(long_spring_grades)))) %>% 
      mutate(SCHLYR=yr,
             CN3=ifelse(SCHLYR %in% c(2008,2009),as.numeric(CN),as.numeric(CN3)),
             SID=ifelse(SCHLYR %in% c(2008,2009),as.numeric(STUID),as.numeric(SID)),
             LEVEL=ifelse(SCHLYR %in% c(2008,2009),LEV,LEVEL),
             TITLE=ifelse(SCHLYR %in% c(2008,2009),SUB,TITLE),
             CRATTX8=ifelse(SCHLYR %in% c(2008,2009),CRD,CRATTX8)) %>%
      select(SID, SCHLID, FMK,CRATTX8,LEVEL,CN3, CN4, CN, TITLE,
             SECTION, TID, TIDM1, TIDM2) %>%
      mutate(fall_semester = 0)
    
    new <- bind_rows(long_fall_grades, long_spring_grades) %>% 
      filter(!is.na(FMK) & FMK != "" & FMK != "/") %>%
      mutate(GRADE_YEAR_FALL = yr)
    names(new) <- toupper(names(new))
    
    new <- new %>%
      select(SID, GRADE_SCHLID = SCHLID, FMK, CRATTX8, LEVEL, CN3, CN4, CN, TITLE, FALL_SEMESTER, GRADE_YEAR_FALL,
             SECTION, TID, TIDM1, TIDM2)
    
    # conbine each year's cleaned grade files into the final dataframe
    grades <- bind_rows(grades, new) 
  }
  return(grades)
}

### Section 2 - Other useful functions. 

#' This function is the equivalent of dresiduals in Stata, which returns the data
#' frame with residuals net of Teacher fixed effects. This is the Chetty style as well.
#' Q: Is this right?
#' 
#' @param dataset Analytic dataset. 
#' @param outcome The outcome we want demeaned. 
#' @param controls Vector of controls (already pasted with +)
#' @return The analytic dataset, but with the dresid style r residuals.  

dresid <- function(dataset, outcome, controls) {
  dataset <- dataset %>%
    filter(across(.cols = outcome, .fns = ~ !is.na(.x)))
  main_reg <- plm(paste0(outcome, " ~ ", controls),
                  index = c("TID"),
                  method = "between",
                  data = dataset)
  full_resid <- data.frame(temp_resid = main_reg$residuals)
  fixed_effects <- fixef(main_reg, type = "dmean")
  fixed_effects <- data.frame(TID = as.integer(as.character(names(fixed_effects))), 
                              temp_value = fixed_effects, row.names = NULL) 
  return_data <- dataset %>%
    left_join(fixed_effects, by = "TID") %>%
    cbind(full_resid) %>%
    mutate(
      "{outcome}_dresid" := temp_value + temp_resid
    ) %>%
    select(-c(temp_value, temp_resid))
  
  return(return_data)
}
