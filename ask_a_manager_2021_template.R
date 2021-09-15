library(googlesheets4)
library(tidyverse)
library(janitor) 
library(skimr)
library(countrycode) # to clean up country names
library(broom)
library(car)
library(ggfortify)

# use googlesheets4 to get data
url <- "https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792"
googlesheets4::gs4_auth() # google sheets authorisation

# load "Ask a A Manager 2021 Survey" googlesheet
# https://www.askamanager.org/
ask_a_manager_2021 <- googlesheets4::read_sheet(url) %>% 
  janitor::clean_names()

# if googlesheets is now working, read local copy
ask_a_manager_2021 <- read_csv(here::here("data", "ask_a_manager_2021.csv"))
  

skimr::skim(ask_a_manager_2021)

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