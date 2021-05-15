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

### 1. Load Libraries and utility functions
library(tidyverse)
library(haven)
library(dplyr)
library(janitor)
library(stringr)
library(eeptools)
source("utils.R")

### 2. Set Time Period for Analysis
first_year <- 2010
last_year <- 2014
year_list <- seq(first_year, last_year, 1)

### 3. Import Data

### 3a. Load Freshmen cohorts for the years under consideration. 
###     This cohort makes up the base group for our analysis. 
### Decision Rules:
###     ? Using 2012 ACS to measure poverty (rather than 2015?)
###     ? gradSchoolTypeNum6yr < 4 to drop charter/alternative students

### Do base Data Cleaning
fclasses <- read.csv("/home/projects/To_and_Through/DATA/newFcohortsOutput/allFcohorts2020_2_9_21.csv") %>%
  filter(freshCohort >= first_year & freshCohort <= last_year) %>%
  mutate(
    cRace = ifelse(cRace == "Unknown Race/Ethnicity", "Other", cRace)
  )
  

### Add Census Data on Poverty
census_data <- read_sas("/mnt/data/demographics/census/censb12.sas7bdat") %>%
  select(CENBLOCK10, PBPOV, PNWORKT)
fclasses <- fclasses %>%
  left_join(census_data, by = "CENBLOCK10") 
  

### 3b. Load Grade Data. 
###     This data contains, on each line, an entry for one class a student was enrolled in. 
###     Additionally, this data is loaded by semester, so we need to merge the two semesters worth of data.
### DATA DECISIONS:
###     - Limiting to only core courses. 
###     ? Limiting only to classes taken for credit. My hope is that this will provide a more
###       accurate sample of the courses we wish to consider. 
###     ? Account for level of class by adjusting numeric GPA encoding. ADDITIONALLY, not increasing 
###       the value for a D or F (this is the way CPS does GPA calculations, but is it correct here?) 

### Step 1. Using function read_grades (see utils.R for details)
raw_grades <- read_grades(first_year, last_year + 1)

### Step 2. Limit to only Core Classes taken for credit, and encode which subject each class represents
grades <- raw_grades %>%
  # Limit to ONLY core classes
  filter(
    CN3 >= 100 & CN3 < 500,
    CRATTX8 > 0
  ) %>%
  # Limit to students in our sample
  filter(SID %in% fclasses$SID) %>%
  # Group classes by the subject to which they fall. 
  mutate(
    subject = ifelse(CN3 >= 100, "English", NA),
    subject = ifelse(CN3 >= 200, "Social Studies", subject),
    subject = ifelse(CN3 >= 300, "Science", subject), 
    subject = ifelse(CN3 >= 400, "Math", subject)
  ) %>%
  # Numerically encode GPAs. Note, adjust the numeric GPA by Honor Level. 
  mutate(
    FMK = ordered(FMK, levels = c("F", "D", "C", "B", "A"), exclude = c("/", "P")),
    
    GPA = factor(FMK, levels = c("/", "P", "A", "B", "C", "D", "F"), labels = c(NA, NA, 4, 3, 2, 1, 0)),
    GPA = as.numeric(as.character(GPA))
    ### USE UNWEIGHTED FOR NOW
    #GPA = ifelse(LEVEL == "A" & (GPA > 1), GPA + 2, GPA),
    #GPA = ifelse(LEVEL == "H" & (GPA > 1), GPA + 1, GPA)
  ) %>%
  # Limit to unique observations
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(GPA))

### Step 3. Deal with Classes with Multiple/Missing Teachers
### Decision Rules:
###   ? For now, leave out classes with more than one teacher
###   ? For now, leave out classes with missing teachers

grades  <- grades %>%
  filter(!(TID == 999999999) & !is.na(TID)) %>%
  select(-c(TIDM1, TIDM2))

### Step 4. Deal with Multiple Observations Per Student/Subject
### Decision Rules (REALLY CHECK THESE):
###   ? Limit for each student/subject each semester to the course code of each subject the
###     student enrolled in which was most popular that semester. Helps get rid of extraneous classes
###     and electives and leave what we would think of as the main CPS curriculum
###     (TO THINK ABOUT: How does dropping some teachers impact our study?)
###   ? For multiple observations of the same course which vary only by title, collapse. 

grades <- grades %>%
  group_by(subject, CN4, GRADE_YEAR_FALL, FALL_SEMESTER) %>%
  mutate(n_taking_this_class = n()) %>%
  ungroup() %>%
  group_by(SID, subject, GRADE_YEAR_FALL, FALL_SEMESTER) %>%
  top_n(1, n_taking_this_class) %>%
  ungroup() 

### Step 5. Get to one observation per student per subject per semester. 
### Decision Rules:
###   ? Collapse observations which differ only by subject. 
###   ? Drop students who are reported to be enrolled in the same class in two different
###     sections with two different teachers (very small sample ~ .1%)

grades <- grades %>%
  select(-TITLE) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(SID, subject, FALL_SEMESTER, GRADE_YEAR_FALL) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(-n_taking_this_class)

### Step 6. Get differenced grades by cohort. 

### Construct fall grades for sophmore year (t2), indexed by freshman cohort year
fall_grades <- grades %>%
  filter(
    FALL_SEMESTER == 1
  ) %>%
  mutate(
    FRESH_COHORT_YEAR = GRADE_YEAR_FALL - 1
  ) %>%
  select(
    SID, 
    FRESH_COHORT_YEAR,
    SOPH_FALL_GPA = GPA,
    SOPH_FALL_FMK = FMK, 
    SOPH_FALL_LEVEL = LEVEL,
    SOPH_TID = TID,
    subject
  ) %>%
  filter(FRESH_COHORT_YEAR >= first_year & FRESH_COHORT_YEAR <= last_year)

### Construct spring grades for freshman year (t1), indexed by freshman cohort year
spring_grades <- grades %>%
  filter(
    FALL_SEMESTER == 0
  ) %>%
  mutate(
    FRESH_COHORT_YEAR = GRADE_YEAR_FALL
  ) %>%
  select(
    SID, FRESH_COHORT_YEAR, GRADE_SCHLID, 
    LEVEL, CN3, SECTION, TID, subject, FRESH_SPRING_GPA = GPA,
    FRESH_SPRING_LEVEL = LEVEL,
    FRESH_SPRING_FMK = FMK
  ) %>%
  filter(FRESH_COHORT_YEAR >= first_year & FRESH_COHORT_YEAR <= last_year)

### Merge fall and Spring Grades, and take difference in 
### Decision Rules:
###   ? Using inner join to keep only students who, for a given subject, appear in both periods
differenced_grades <- spring_grades %>%
  inner_join(fall_grades, by = c("SID", "FRESH_COHORT_YEAR", "subject")) %>%
  mutate(
    grade_difference = FRESH_SPRING_GPA - SOPH_FALL_GPA
  )
  

### 3c. 8th Grade Discipline Data 
discipline <- data.frame()
for(yr in year_list) {
  yr_string_discipline <- paste0(substr(yr-1, 3, 4), substr(yr, 3, 4))
  discipline_temp <- read_sas(paste0("/mnt/data/discipline/misconduct", yr_string_discipline, ".sas7bdat"))
  discipline_temp <- discipline_temp %>%
    clean_names(case = "small_camel") %>%
    group_by(sid) %>%
    summarize(
      n_infractions_grade_8 = n()
    ) %>%
    mutate(
      FRESH_YEAR = yr
    )
  discipline <- bind_rows(discipline, discipline_temp)
}

### 3d. 8th Grade Test Data
### Data Decisions:
###   ? Standardizing scores within year/grade
test_data <- data.frame() 
for(yr in year_list) {
  # If fall of 8th grade year was 2013 or before USE ISAT
  if((yr - 1) <= 2013) {
    test_temp <- read_sas(paste0("/mnt/data/CPS_test_data/ISAT/ISAT2006/isat", 
                                 substr(yr, 3, 4), ".sas7bdat")) %>%
      filter(GRADE == 8) %>%
      select(YEAR, SID, READ_SS, MATH_SS) %>%
      mutate(
        READ_Z = (READ_SS - mean(READ_SS, na.rm = TRUE)) / sd(READ_SS, na.rm = TRUE), 
        MATH_Z = (MATH_SS - mean(MATH_SS, na.rm = TRUE)) / sd(MATH_SS, na.rm = TRUE)
      ) %>%
      select(-c(READ_SS, MATH_SS))
  }
  # If fall of 8th grade year was from 2014 to 2017
  if((yr - 1) >= 2014 & (yr-1) <= 2017) {
    test_temp <- read_sas(paste0("/mnt/data/CPS_test_data/PARCC/parcc", 
                                 substr(yr, 3, 4), ".sas7bdat")) %>%
      filter(MT_TEST == "MAT08" & EN_TEST == "ELA08") %>%
      select(YEAR, SID, MT_SCORE, EN_SCORE) %>%
      mutate(
        READ_Z = (EN_SCORE - mean(EN_SCORE, na.rm = TRUE)) / sd(EN_SCORE, na.rm = TRUE),
        MATH_Z = (MT_SCORE - mean(MT_SCORE, na.rm = TRUE)) / sd(MT_SCORE, na.rm = TRUE)
      ) %>%
      select(
        -c(EN_SCORE, MT_SCORE)
      )
  }
  test_data <- bind_rows(test_data, test_temp)
}

### 3e. Other Masterfile Info: Birthdates
### Load birthdate date
### Get age at September 1st of each year in study
birthday_data <- read_sas("/mnt/data/masterfile_data/allm520.sas7bdat", col_select = c("SID", "BDATE520")) %>%
  mutate(
    BDATE520 = as.character(BDATE520),
    birthdate1 = ifelse(str_length(BDATE520) == 5, 
                      paste0("0", BDATE520),
                      BDATE520),
    birthdate2 = as.Date(birthdate1, "%m%d%y"),
    birthdate = as.Date(ifelse(birthdate2 > "2015-12-31", format(birthdate2, "19%y-%m-%d"), format(birthdate2)))
  ) %>%
  select(SID, birthdate)

ages <- data.frame()
for(yr in year_list) {
  yr_start_date <- as.Date(paste0(yr, "-09-01"))
  birthday_data_temp <- birthday_data %>%
    filter(SID %in% fclasses$SID) %>%
    mutate(
      YEAR = yr, 
      age = ifelse(yr_start_date >= birthdate, 
                   floor(age_calc(enddate = yr_start_date, dob = birthdate, units = "years")),
                   NA)
    )
  ages <- bind_rows(ages, birthday_data_temp)
}

### 4. Combine Datasets - Define Cohort

### 4a. Linking All Individual Level Data
student_vals_raw <- fclasses %>%
  left_join(discipline, by = c("SID" = "sid", "freshCohort" = "FRESH_YEAR")) %>%
  mutate(
    n_infractions_grade_8 = replace_na(n_infractions_grade_8, 0)
  ) %>%
  left_join(test_data, by = c("SID", "freshCohort" = "YEAR")) %>%
  left_join(ages, by = c("SID", "freshCohort" = "YEAR"))
student_vals_raw <- student_vals_raw %>%
  mutate(
    dFreshSped = ifelse(cSped == "None", 0, 1)
  )

### 4b. Cleaning Cohort
### DATA DECISIONS (? means still to check):
###     ? Not dropping freshmen year SPED students. This may violate our assumptions because they will likely
###       be assigned to a given teacher based on underlying characteristics?
###     ? Limit to students in Freshmen denominator. This drops students who weren't in a standard
###       CPS highschool for both semesters of freshmen year (dropping in/out transfers and students at
###       charter/alternative schools). I don't think this violates any assumptions, unless these studnets
###       leave because of a negative experience with a given teacher, which seems not likely on a large scale. 
###     ? Drop freshmen dropout. This seems potentially an issue, but I don't see a way around it, for our 
###       analysis. Kind of has the same assumption as the above. 
###     ? Drop students who left during freshmen year. Same as above.


student_vals <- student_vals_raw %>%
  filter(dInFreshmenDenominator == 1) %>%
  filter(dFreshDropOut == 0) %>%
  filter(dFreshValidLvRea == 0)

### 4c. Filter for Incomplete Observations of main Variables
### Decision Rules:
###     ? Is this right? Drops around 3000.
student_vals <- student_vals %>%
  filter(!is.na(cRace) &
         !is.na(cGender) &
         !is.na(age) &
         !is.na(rnoAttend) &
         !is.na(n_infractions_grade_8) &
         !is.na(MATH_Z) &
         !is.na(READ_Z) &
         !is.na(rnoCoreGpa) &
         !is.na(cSped) & 
         !is.na(PBPOV))


### 4c. Linking Individual Data to Grades Data
### Decision Rules:
###   ? Using an inner join - is this the best way to go about it? Keep only students 
###     who satisfy all grade decision rules. 
analytic_dataset <- differenced_grades %>%
  inner_join(student_vals, by = c("SID", "FRESH_COHORT_YEAR" = "freshCohort"))


### 4d. Drop teachers who taught at more than one school in a given year
analytic_dataset <- analytic_dataset %>%
  group_by(TID, FRESH_COHORT_YEAR) %>% 
  filter(length(unique(GRADE_SCHLID)) == 1)

### 4e. Sped Controls (Following Chetty)
analytic_dataset <- analytic_dataset %>% 
  group_by(TID, FRESH_COHORT_YEAR, GRADE_SCHLID) %>%
  filter(sum(dFreshSped) / n() < .25)
  
### 4f. Controls for how many students a teacher sees TOTAL. 
### Here we don't exactly follow Chetty because many students in CPS 
### are in 1-7 person classes. I believe this could be an administrative 
### side effect of the same class being listed in multiple sections. 
### TODO: Think about this further.
analytic_dataset <- analytic_dataset %>%
  group_by(TID, FRESH_COHORT_YEAR) %>%
  filter(n() >= 7) %>%
  ungroup()

analytic_cohort <- student_vals %>%
  filter(SID %in% analytic_dataset$SID)
raw_cohort <- student_vals_raw 



### 5a. Class Level Controls
analytic_dataset <- analytic_dataset %>%
  group_by(GRADE_SCHLID, TID, SECTION, FRESH_COHORT_YEAR) %>%
  mutate(
    class_MATH_Z = mean(MATH_Z),
    class_READ_Z = mean(READ_Z),
    class_age = mean(age),
    class_rnoAttend = mean(rnoAttend),
    class_n_infractions_grade_8 = mean(n_infractions_grade_8),
    class_rnoCoreGpa = mean(rnoCoreGpa),
    class_size = n(),
    class_pbpov = mean(PBPOV)
  ) %>%
  ungroup()


### 5b. School Level Controls
### Get grade vs funit figured out?
analytic_dataset <- analytic_dataset %>%
  group_by(GRADE_SCHLID, FRESH_COHORT_YEAR) %>%
  mutate(
    school_MATH_Z = mean(MATH_Z),
    school_READ_Z = mean(READ_Z),
    school_age = mean(age),
    school_rnoAttend = mean(rnoAttend),
    school_n_infractions_grade_8 = mean(n_infractions_grade_8),
    school_rnoCoreGpa = mean(rnoCoreGpa),
    school_size = n(),
    school_pbpov = mean(PBPOV)
  ) %>%
  ungroup()


### Q? - Class Size Limitations (or still to do)

### 6. Write Output
write_rds(analytic_dataset, file = "../Output/analytic_dataset.rds")
write_rds(raw_cohort, file = "../Output/raw_cohort.rds")
write_rds(analytic_cohort, file = "../Output/analytic_cohort.rds")
