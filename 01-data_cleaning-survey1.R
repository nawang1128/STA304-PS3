#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 'voter study group' [...UPDATE ME!!!!!]
# Author: Ruotong Wang and Shengnan Mao [CHANGE THIS TO YOUR NAME!!!!]
# Data: 30 October 2020
# Contact: ruotong.wang@mail.utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/Nancy/Desktop/ns20200625")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("ns20200625.dta")
# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables
reduced_survey <- 
  raw_data_survey %>% 
  select(vote_intention,
         vote_2020,
         age,
         gender,
         state,
         employment,
         race_ethnicity,
         household_income,
         education)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
reduced_survey$age<-as.numeric(reduced_survey$age)

#filter people intented to vote and assume people will vote unless they say no
filtered_survey<-reduced_survey %>% 
  filter(vote_intention!="No, I am not eligible to vote"&
           vote_intention!="No, I will not vote but I am eligible"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
  )

#reduce all na
filtered_survey<-na.omit(filtered_survey)

#create age groups
filtered_survey <- filtered_survey %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 40 ~ '21 to 40',
                              age >40  & age <= 60 ~ '41 to 60',
                              age >60  & age <= 80 ~ '61 to 80',
                              age >80 ~ 'above 80'
  )) 
unique(filtered_survey$agegroup)

#gender
unique(filtered_survey$gender)

#education
unique(filtered_survey$education)
filtered_survey$education[filtered_survey$education == "Other post high school vocational training"] <- "High school graduate"
filtered_survey$education[filtered_survey$education == "Completed some graduate, but no degree"] <- "College Degree (such as B.A., B.S.)"
unique(filtered_survey$education)

#state
unique(filtered_survey$state)

#income
unique(filtered_survey$household_income)

#race
length(unique(filtered_survey$race_ethnicity))
otherasian <- c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)",
                "Asian (Filipino)","Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")
filtered_survey <- filtered_survey %>% 
  mutate(race = case_when(race_ethnicity == "Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity == "Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% otherasian ~"Other Asian or Pacific Islander",
                          race_ethnicity == "White" ~ 'White',
                          race_ethnicity == "Black, or African American" ~ 'Black, or African American',
                          race_ethnicity == "Some other race" ~ 'Other race',
                          race_ethnicity == "American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity == "Other race "~"Other race"
  )) 
filtered_survey$race_ethnicity <- NULL

#employment
filtered_survey <- rename(filtered_survey, employment2 = employment)
filtered_survey <- filtered_survey %>% 
  mutate(employment = case_when(employment2 == "Full-time employed" ~ 'Employed',
                                employment2 == "Self-employed" ~ 'Employed',
                                employment2 == "Part-time employed" ~ 'Employed',
                                employment2 == "Unemployed or temporarily on layoff" ~ 'Unemployed',
                                employment2 == "Retired" ~ 'Not in Labor Force',
                                employment2 == "Student" ~ 'Not in Labor Force',
                                employment2 == "Homemaker" ~ 'Not in Labor Force',
                                employment2 == "Permanently disabled" ~ 'Not in Labor Force',
                                employment2 == "Other:"~ 'Not in Labor Force'
  )) 
unique(filtered_survey$employment)
filtered_survey$employment2 <- NULL

#Populate Dataset
filtered_survey %>% select(vote_2020, age, agegroup, gender, state, employment, race, household_income, education) -> survey

#create cells
survey$cell <- paste(survey$gender, survey$race)

#convert var to factors
factor.survey <- c("agegroup", "gender", "education", "employment", "state", "household_income", "race", "cell", "vote_2020")
survey[factor.survey] <- lapply(survey[factor.survey], factor) 

#To predict probability of voting for Trump (Biden as ref)
survey$vote_2020 <- relevel(survey$vote_2020, ref = "Joe Biden")

#Count number of cells
length(unique(survey$cell))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(survey, "/Users/Nancy/Desktop/survey.csv")



