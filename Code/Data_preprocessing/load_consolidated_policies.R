library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")
library("readr")
library("janitor")
library("lubridate")
library("here")
library("stringr")

rm(list=ls())

cons_policies <- read_excel(here::here("Data","Policies","consolidated_policies_source.xlsx"))
cons_policies <- cons_policies %>% janitor::clean_names()
cons_policies <- cons_policies %>%
  mutate(announced_date = dmy(announced_date),
         start_date = dmy(start_date),
         end_date = dmy(end_date),
         national = if_else(canton=="ALL", TRUE, FALSE)
  )

# only pick major events
cons_policies <- cons_policies %>% filter(include)

# load all canton codes
canton_codes <- read_csv(here::here("Data","Canton_codes.csv"))

# replace "ALL" cantons with each canton 
all_cantons <- canton_codes %>% dplyr::select(canton_codes) %>%
  mutate(region="ALL")
cons_policies <- left_join(cons_policies,
                            all_cantons, c("canton"="region")) %>%
  mutate(canton=if_else(canton=="ALL", canton_codes, canton)) %>%
  dplyr::select(-canton_codes)

# now add canton names
cons_policies <- left_join(cons_policies,
                           canton_codes %>% dplyr::select(-MIP_canton_names),
                           c("canton"="canton_codes"))  

# use start date
cons_policies <- cons_policies %>% 
        mutate(date = start_date,
               date = if_else(is.na(start_date) & !is.na(announced_date), announced_date, start_date))

# select and arrange
cons_policies <- cons_policies %>% dplyr::select(id,announced_date,
                                                 start_date,end_date,date,
                                                 canton,canton_names,
                                                 everything())
cons_policies <- cons_policies %>% arrange(canton,date)

# cantonal policy vs national policy: use cantonal policy
cons_policies <- cons_policies %>% 
      group_by(canton, policy_short) %>%
      mutate(both_national_regional = length(unique(national))) %>%
      ungroup() %>%
      filter(national==FALSE | both_national_regional != 2)
      
# check if there are more duplicates
cons_policies %>%
  group_by(policy_short, canton) %>%
  mutate(count=n()) %>%
  ungroup() %>%
  filter(count>1)
 
# create full panel 
cons_policies_full <- 
  cons_policies %>% 
          dplyr::select(canton,canton_names, policy_short, date) %>%
          mutate(value=TRUE) %>%
          group_by(canton, canton_names, policy_short) %>%
          complete(date = seq(as.Date("2020-02-15"), as.Date("2020-05-23"), by="day")) %>%
          fill(value, .direction="down") %>%
          ungroup() %>%
          mutate(value = if_else(is.na(value), FALSE, value)) 
cons_policies_full <- cons_policies_full %>% 
  pivot_wider(names_from=policy_short, values_from=value)

## saving
write_csv(cons_policies_full, path=here::here("Data","Policies","consolidated_policies.csv"))
