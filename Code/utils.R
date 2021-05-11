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

dresid_2 <- function(input_dataset, outcome, controls, subjects) {
  return_data <- data.frame()
  for(current_sub in subjects) {
    dataset <- input_dataset %>%
      filter(subject == current_sub) %>%
      filter(across(.cols = outcome, .fns = ~ !is.na(.x)))
    main_reg <- plm(paste0(outcome, " ~ ", controls),
                    index = c("TID", "SID"),
                    model = "within",
                    data = dataset
                )
    residuals <- cbind(residuals = as.vector(main_reg$residuals),
                       attr(main_reg$residuals, "index")) %>%
      mutate(
        TID = as.integer(as.character(TID)),
        SID = as.integer(as.character(SID))
      )
    fixed_effects <- fixef(main_reg, type = "dmean")
    fixed_effects <- data.frame(TID = as.integer(as.character(names(fixed_effects))), 
                                fixed_effects = fixed_effects, row.names = NULL) %>%
      mutate(fixed_effects = fixed_effects - mean(fixed_effects))
    return_data <- rbind(return_data, fixed_effects)
  }
  return(return_data)
}


dresid <- function(input_dataset, outcome, controls, subjects, year_list) {
  return_data <- data.frame()
  for(current_yr in year_list) {
    for(current_sub in subjects) {
      dataset <- input_dataset %>%
        filter(subject == current_sub) %>%
        filter(FRESH_COHORT_YEAR == current_yr) %>%
        filter(!is.na(!!rlang::sym(outcome)))
      resid_reg <- plm(paste0(outcome, " ~ ", controls),
                       index = c("TID", "SID"),
                       model = "within",
                       data = dataset
      )
      fixed_effects <- fixef(resid_reg, type = "dmean")
      fixed_effects <- data.frame(TID = as.integer(as.character(names(fixed_effects))), 
                                  fixed_effects_var = fixed_effects, row.names = NULL) %>%
        mutate(fixed_effects = fixed_effects - mean(fixed_effects))
      
      residuals <- cbind(residuals_var = as.vector(resid_reg$residuals),
                         attr(resid_reg$residuals, "index")) %>%
        mutate(
          TID = as.integer(as.character(TID)),
          SID = as.integer(as.character(SID))
        )
      
      
      
      dataset <- dataset %>%
        ungroup() %>%
        left_join(fixed_effects, by = "TID") %>%
        left_join(
          residuals, by = c("SID", "TID")
        ) %>%
        mutate(
          dresiduals = residuals_var + fixed_effects_var
        )
      
      re_glm <- lmer(dresiduals ~ (1 | TID), data = dataset)
      re <- ranef(re_glm)$TID 
      re <- data.frame(TID = rownames(re), re = re$`(Intercept)`, row.names = NULL) %>%
        mutate(TID = as.numeric(as.character(TID)))
      
      effects <- fixed_effects %>%
        left_join(re, by = "TID") %>%
        mutate(
          subject = current_sub,
          FRESH_COHORT_YEAR = current_yr
        )
      
      return_data <- rbind(return_data, effects)
    }
  }
  return(return_data)
}
