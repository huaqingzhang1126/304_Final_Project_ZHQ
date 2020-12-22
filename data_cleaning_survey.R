#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape ¡®Full Data Set¡¯
# Author: Huaqing Zhang
# Data: December 22, 2020
# Contact: huaqing.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!

#### Workspace setup ####
library(plyr)
library(haven)
library(tidyverse)
library(cesR)
library(labelled)

#install.packages("devtools")
#devtools::install_github("hodgettsp/cesR")

setwd("C:/Sarah/third-Fall/STA304/Final project")
# Add the labels
get_ces("ces2019_web")
ces2019_web<-to_factor(ces2019_web)
# Just keep some variables
survey_data <- select(ces2019_web,
                      cps19_yob,
                      cps19_gender,
                      cps19_province,
                      cps19_education, 
                      cps19_votechoice
                      )

survey_data <-
  survey_data %>%
  filter(cps19_votechoice != "Don't know/ Prefer not to answer")%>%
  filter(cps19_gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)")%>%
         filter(cps19_province != "Yukon")


survey_data<-
  survey_data %>%
  mutate(age = 2019 - as.integer(survey_data$cps19_yob)-1919)

survey_data<-
  survey_data %>%
  mutate(sex = 
           ifelse(cps19_gender == "A man", "Male", "Female"))

survey_data<-
  survey_data %>%
  mutate(vote_liberal = 
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))

survey_data<-
  survey_data %>%
  mutate(province = cps19_province)

survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Trade certificate or diploma"= "University certificate or diploma below the bachelor's level"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("No schooling"="Less than high school diploma or its equivalent"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Some elementary school"="Less than high school diploma or its equivalent"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Completed elementary school"="Less than high school diploma or its equivalent"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Some secondary/ high school"="Less than high school diploma or its equivalent"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Some technical, community college, CEGEP, College Classique"="High school diploma or a high school equivalency certificate"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Completed secondary/ high school"="High school diploma or a high school equivalency certificate"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Completed technical, community college, CEGEP, College Classique"="College, CEGEP or other non-university certificate or di..."))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Some university"="University certificate or diploma below the bachelor's level"))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Master's degree"="University certificate, diploma or degree above the bach..."))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Professional degree or doctorate"="University certificate, diploma or degree above the bach..."))
survey_data$cps19_education <- 
  revalue(survey_data$cps19_education, c("Bachelor's degree"="Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"))


survey_data <- survey_data %>%
  mutate(education = cps19_education)

survey_data <- survey_data %>%
  filter(education != "Don't know/ Prefer not to answer")

survey_data <- select(survey_data,
                      age,
                      sex,
                      province,
                      education,
                      vote_liberal)

survey_data $province <- as.character(survey_data $province)
survey_data $education <- as.character(survey_data $education)

table(survey_data$province)
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(survey_data, "survey_data.csv")

