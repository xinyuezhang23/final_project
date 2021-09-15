library(googlesheets4)
library(tidyverse)
library(janitor) 
library(skimr)
library(countrycode) # to clean up country names
library(broom)
library(car)
library(ggfortify)
library(stringr)

# use googlesheets4 to get data
# url <- "https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792"
# googlesheets4::gs4_auth() # google sheets authorisation

# load "Ask a A Manager 2021 Survey" googlesheet
# https://www.askamanager.org/
# ask_a_manager_2021 <- googlesheets4::read_sheet(url) %>% 
#   janitor::clean_names()

# if googlesheets is now working, read local copy
ask_a_manager_2021 <- read_csv(here::here("data", "ask_a_manager_2021.csv")) %>% 
  janitor::clean_names()
  

skimr::skim(ask_a_manager_2021)

#data cleaning
ask_a_manager_2021_new <- ask_a_manager_2021 %>% 
  rename(age=how_old_are_you,overall_experience=overall_years_of_professional_experience,
         field_experience=years_of_experience_in_field,education_level=highest_level_of_education_completed) %>% 
  select(-timestamp,-additional_context_on_job_title ,-additional_context_on_income,-other_monetary_comp,-currency_other) %>% 
  mutate(age=factor(age,levels=c("under 18","18-24","25-34","35-44","45-54","55-64","65 or over"),
                    labels =c("under 18","18-24","25-34","35-44","45-54","55-64","65 or over") ,ordered = TRUE),
         overall_experience=factor(overall_experience,levels=c("1 year or less","2 - 4 years","5-7 years","8 - 10 years",
                                                               "11 - 20 years","21 - 30 years","31 - 40 years","41 years or more"),
                    labels =c("less than 1 year","2-4 years","5-7 years","8-10 years",
                              "11-20 years","21-30 years","31-40 years","41 years or more") ,ordered = TRUE),
         field_experience=factor(field_experience,levels=c("1 year or less","2 - 4 years","5-7 years","8 - 10 years",
                                                               "11 - 20 years","21 - 30 years","31 - 40 years","41 years or more"),
                                   labels =c("less than 1 year","2-4 years","5-7 years","8-10 years",
                                             "11-20 years","21-30 years","31-40 years","41 years or more") ,ordered = TRUE),
         education_level=factor(education_level,levels=c("High School","Some college","College degree",
                                                         "Master's degree","PhD","Professional degree (MD, JD, etc.)"),
                                labels =c("High School","Some college","College degree",
                                          "Master's degree","PhD","Professional degree") ,ordered = TRUE),
         gender=case_when(gender=="Other or prefer not to answer"|gender=="Prefer not to answer"|is.na(gender)~"Don't know or NA",
                          TRUE~gender),state=ifelse(str_detect(state,",")|is.na(state),"NA",state))
#sort out country
country_iso3 <-  ask_a_manager_2021_new %>% 
  select(country) %>% 
  pull() %>% 
  countrycode(
    origin = "country.name",
    destination = 'iso3c') %>% 
  as_tibble()
#bind them with right country codes
ask_a_manager_2021_new <- bind_cols(ask_a_manager_2021_new, country_iso3)

# How is salary distributed?

ggplot(ask_a_manager_2021, aes(x=annual_salary))+
  geom_density()


ggplot(ask_a_manager_2021, aes(x=annual_salary))+
  stat_ecdf()

# what about log(salary)? 
ggplot(ask_a_manager_2021, aes(x=log(annual_salary)))+
  geom_density()


ggplot(ask_a_manager_2021, aes(x=log(annual_salary)))+
  stat_ecdf()


# Which one (salary vs. log(salary)) is better to use in a regression model? Why?


# Some quick counts, groups, etc

ask_a_manager_2021 %>% 
  count(how_old_are_you, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n))


# Industry is messy... it has > 1000 different industries  
ask_a_manager_2021 %>% 
  count(industry, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n))

# Most of 'currency' is USD
ask_a_manager_2021 %>% 
  count(currency, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n))

# 'country' 
ask_a_manager_2021 %>% 
  count(country, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n))

# use countrycode::countryname() to clean country names


# 'city' 
ask_a_manager_2021 %>% 
  count(city, sort=TRUE) %>% 
  mutate(percent = 100* n/sum(n))

# highest_level_of_education_completed 
ask_a_manager_2021 %>% 
  count(highest_level_of_education_completed) %>% 
  mutate(percent = 100* n/sum(n))%>% 
  arrange(desc(percent))

# gender
ask_a_manager_2021 %>% 
  count(gender) %>% 
  mutate(percent = 100* n/sum(n))%>% 
  arrange(desc(percent))

# race
ask_a_manager_2021 %>% 
  count(race) %>% 
  mutate(percent = 100* n/sum(n))%>% 
  arrange(desc(percent))

# overall_years_of_professional_experience 
ask_a_manager_2021 %>% 
  count(overall_years_of_professional_experience ) %>% 
  mutate(percent = 100* n/sum(n))%>% 
  arrange(desc(percent))

# years_of_experience_in_field  
ask_a_manager_2021 %>% 
  count(years_of_experience_in_field  ) %>% 
  mutate(percent = 100* n/sum(n))%>% 
  arrange(desc(percent))