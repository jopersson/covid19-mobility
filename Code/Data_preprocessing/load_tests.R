
## write testing data ## 

library("here")
library("readxl")
library("dplyr")
library("stringr")
library("readr")

rm(list=ls())

# load
dta <- read_excel(here::here("Data","BAG_covid","Dashboard_3_COVID19_labtests_positivity.xlsx"))

# count positive tests
dta$Outcome_tests <- ifelse(dta$Outcome_tests == "Positive", dta$Number_of_tests, 0)

# mutate test data
dta <- dta %>% 
  select(Datum, Number_of_tests, Outcome_tests) %>%
  mutate(Datum = str_remove(Datum, "[ UTC]")) %>%
  group_by(Datum) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(positivity_rate = Outcome_tests/Number_of_tests) %>% 
  select(Datum, Number_of_tests, positivity_rate)

colnames(dta)<- c("date","tests","positivity_rate")

# data prior to testing begun
date_range <- c(seq(as.Date("2019-12-30"), by = "day", length.out = 25), as.Date("2020-01-26"), as.Date("2020-02-17"))
zeros1 <- rep(0,length(date_range))
zeros2 <- rep(0,length(date_range))

# create data frame 
pre_dta <- data.frame(date_range, zeros1, zeros2)
colnames(pre_dta) <- c("date","tests","positivity_rate")

# merge data frames
dta <- rbind(pre_dta,dta)

# order by date
dta <- dta[order(as.Date(dta$date, format="%Y-%m-%d")),]

# write file
write_csv(dta, path=here::here("Data","BAG_covid","tests.csv"))

