#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS [...UPDATE ME!!!!!]
# Author: Ruotong Wang and Shengnan Mao [CHANGE THIS TO YOUR NAME!!!!]
# Data: 30 October 2020
# Contact: ruotong.wang@mail.utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/Nancy/Desktop/usa")
raw_data_census <- read_dta("usa_00001.dta")

# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_census <- 
  raw_data_census %>% 
  select(perwt,
         age,
         sex,
         stateicp,
         empstatd,
         race,
         educd)
         
#### What's next? ####
reduced_census$age<-as.numeric(reduced_census$age)

#filter the people who can vote
#(without people who are younger than 18 and whose total income is less than 0)
filtered_census<-reduced_census %>% filter( age >= 18 )

filtered_census<-na.omit(filtered_census)

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

#create age groups
filtered_census<-filtered_census %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 40 ~ '21 to 40',
                              age >40  & age <= 60 ~ '41 to 60',
                              age >60  & age <= 80 ~ '61 to 80',
                              age >80 ~ 'above 80'
  )) 
unique(filtered_census$agegroup)

#sex and rename
unique(filtered_census$sex)
filtered_census$sex <- ifelse(filtered_census$sex == "female", "Female", "Male")
filtered_census <- rename(filtered_census, gender = sex)
unique(filtered_census$gender)

#education
unique(filtered_census$educd)
grade3.less <- c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade4to8 <- c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade9to11 <- c("grade 9","grade 10","grade 11","12th grade, no diploma")
edu.highsch <- c("ged or alternative credential","regular high school diploma")
edu.somecoll <- c("some college, but less than 1 year","1 or more years of college credit, no degree")
filtered_census<-filtered_census %>% 
  mutate(education = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~'Doctorate degree',
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% edu.somecoll~"Completed some college, but no degree",
                            educd %in% edu.highsch~"High school graduate",
                            educd %in% grade9to11~"Completed some high school",
                            educd %in% grade4to8~"Middle School - Grades 4 - 8",
                            educd %in% grade3.less ~"3rd Grade or less"
  )) 
unique(filtered_census$education)

#state
filtered_census<-filtered_census %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC")) 
filtered_census$stateicp <- NULL
unique(filtered_census$state)

#race
length(unique(filtered_census$race))
filtered_census <- rename(filtered_census, race2 = race)
filtered_census<-filtered_census %>% 
  mutate(race = case_when(race2 == "white"~"White",
                          race2 == "chinese"~"Chinese",
                          race2 == "black/african american/negro"~"Black, or African American",
                          race2 == "two major races"~"Other race",
                          race2 == "other race, nec"~"Other race",
                          race2 == "japanese"~"Japanese",
                          race2 == "american indian or alaska native"~"American Indian or Alaska Native",
                          race2 == "three or more major races"~"Other race",
                          race2 == "other asian or pacific islander"~"Other Asian or Pacific Islander"
  )) 
filtered_census$race2 <- NULL

#employment
unique(filtered_census$empstatd)
filtered_census <- filtered_census %>% 
  mutate(employment = case_when(empstatd == "at work" ~ 'Employed',
                                empstatd == "armed forces--at work" ~ 'Employed',
                                empstatd == "has job, not working" ~ 'Employed',
                                empstatd == "armed forces--not at work but with job" ~ 'Employed',
                                empstatd == "unemployed" ~ 'Unemployed',
                                empstatd == "not in labor force" ~ 'Not in Labor Force',
  )) 
unique(filtered_census$employment)
filtered_census$empstatd <- NULL

#populate dataset
filtered_census %>% select(perwt, age, agegroup, gender, state, employment, race, education) -> census

#create cells
census$cell <- paste(census$gender, census$race)

#convert var to factors
factor.census <- c("agegroup", "gender", "education", "employment", "state", "race", "cell")
census[factor.census] <- lapply(census[factor.census], factor)

#Count number of cells
length(unique(census$cell))

# Saving the census data as a csv file in my
# working directory
write_csv(census, "/Users/Nancy/Desktop/census.csv")




         