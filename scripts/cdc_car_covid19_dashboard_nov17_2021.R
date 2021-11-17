#Testing with COVID Testing data

install.packages("RcppRoll")

library(RCurl)
library(tidyverse)
library(readr)
library(RcppRoll)
library(lubridate)

#full OUR world in data dataset:
urlfile2 = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
owdcovid <- read.csv(url(urlfile2))
#note full codebook is available here: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv
head(owdcovid)
tail(owdcovid)
str(owdcovid)
class(owdcovid$date)

#filter for central america
central_america <- c("Guatemala","Panama","Belize","El Salvador","Costa Rica","Honduras", "Nicaragua","Dominican Republic")
#adding population data and 14-day change using 7-day smoothed averages (working on an alternative with biweekly sums)
owdcovid_car <-
  owdcovid %>%
  filter(`location` %in% central_america) %>%
  mutate(mydate = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(`location`) %>%
  arrange(mydate, .by_group = TRUE) %>%
  mutate(casos_trend14 = (new_cases_smoothed - lag(new_cases_smoothed, 14)) / lag(new_cases_smoothed, 14)) %>%
  mutate(defunc_trend14 = (new_deaths_smoothed - lag(new_deaths_smoothed, 14)) / lag(new_deaths_smoothed, 14)) %>%
  mutate(roll_sum_cases = roll_sum(new_cases, 14, align = "right", fill = NA)) %>%
  mutate(biweekly_change_cases = (roll_sum_cases - lag(roll_sum_cases, 14)) / lag(roll_sum_cases, 14)) %>%
  mutate(roll_sum_deaths = roll_sum(new_deaths, 14, align = "right", fill = NA)) %>%
  mutate(biweekly_change_deaths = (roll_sum_deaths - lag(roll_sum_deaths, 14)) / lag(roll_sum_deaths, 14))


head(owdcovid_car)
tail(owdcovid_car)
str(owdcovid_car)

write.csv(owdcovid_car, "/Users/Margot Charette/Documents/Data_projects/COVID_tests/owdcovid_car.csv", row.names = FALSE)



