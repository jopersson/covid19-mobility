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

eth_policies <- read_excel(here::here("Data","Policies","ETH_policies_source.xlsx")) %>% dplyr::select(-source)
eth_policies <- eth_policies %>% 
                mutate(start_date = ymd(start_date),
                       end_date = ymd(end_date))

# load all canton codes
canton_codes <- read_csv(here::here("Data","Canton_codes.csv"))

# replace "ALL" cantons with each canton 
all_cantons <- canton_codes %>% dplyr::select(canton_codes) %>%
  mutate(region="ALL")
eth_policies <- left_join(eth_policies,
                           all_cantons, c("region"="region")) 
eth_policies <- eth_policies %>%
  mutate(canton_codes=if_else( region=="ALL", canton_codes, region ))  

# preference of cantonal over regional policy
eth_policies <- eth_policies %>% 
      group_by(canton_codes, policy) %>% 
      arrange(start_date, .by_group=TRUE) %>%
      summarise_all(first) %>% 
      ungroup()
eth_policies <- eth_policies %>% dplyr::select(-region)

# create full panel
eth_policies_full <- eth_policies %>%
    mutate(date = ymd("2020-02-15")) %>%
    group_by(canton_codes, policy, start_date, end_date) %>%
    complete(date = seq(as.Date("2020-02-15"), as.Date("2020-05-25"), by="day")) %>%
    ungroup()

# create policy variable
eth_policies_full <- eth_policies_full %>% 
    mutate(value = case_when(date < start_date ~  FALSE,
                        date >= start_date & is.na(end_date) ~ TRUE, 
                        date >= start_date & date < end_date ~ TRUE, 
                        date >= end_date ~ FALSE))

# to wide format
eth_policies_full <- eth_policies_full %>% 
      dplyr::select(-start_date,-end_date) %>%
      pivot_wider(names_from = policy, values_from=value)

## saving
save(eth_policies_full, file=here::here("Data","Policies","ETH_policies.RData"))
write_csv(eth_policies_full, path=here::here("Data","Policies","ETH_policies.csv"))
