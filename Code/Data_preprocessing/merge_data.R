
## Merge data for analysis

library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("lubridate")
library("here")
library("readxl")
library("haven")

rm(list=ls())

source(here::here("Code","helper_functions.R"))

# load BAG
bag <- read_csv(here::here("Data","BAG_covid","BAG_covid.csv"))

# load policies, excluding Liechtenstein
cons_policies_full <- read_csv(here::here("Data","Policies","Covid_policies_consolidated_full.csv"))
cons_policies_full <- cons_policies_full %>% filter(canton != "FL")
eth_policies_full <- read_csv(here::here("Data","Policies","ETH_policies.csv"))

# load test data
test_dta <- read_csv(here::here("Data","BAG_covid","tests.csv"))

# load cantonal population
canton_pop <- read_csv(here::here("Data","BFS_canton_population","canton_pop_2019.csv"))

# total trips from MIP
mip_wide <- read_csv(file=here::here("Data","MIP_data","processed_MIP_wide.csv"))

### merge BAG and Policies
dta <- full_join(bag, cons_policies_full %>% dplyr::select(-canton_names),
                 c("date"="date","canton_code"="canton"))

### merge with MIP
dta <- full_join(mip_wide, dta %>% dplyr::select(-canton_names),
                 c("date"="date","canton_codes"="canton_code"))

# add ETH policies 
dta <- left_join(dta, eth_policies_full, c("canton_codes"="canton_codes","date"="date" ))

# replace missing with 0/FALSE
dta <- dta %>% mutate_at(vars(starts_with("bag")),
                  list(~if_else(is.na(.), 0, .)))
dta <- dta %>% mutate_at(vars(cl_border:shops_closed),
                         list(~if_else(is.na(.), FALSE, .)))
dta <- dta %>% mutate_at(vars(starts_with("eth")),
                         list(~if_else(is.na(.), FALSE, .)))
# roll up datet0
dta <- dta %>% 
    group_by(canton_codes) %>%
    arrange(date, .by_group=TRUE) %>%
    fill(date_t0, .direction="up") %>%
    ungroup()
# redefine t variable
dta <- dta %>%  mutate(t = date - date_t0,
                       t = as.integer(t) + 1,
                       wd = weekdays(date),
                       sunday = if_else(wd == "Sunday", date, as.Date(NA)))

# reorder
dta <- dta %>% dplyr::select(canton_codes, canton_name, date, date_t0, t, wd, sunday, everything() )

### create dependent variables
dta <- dta %>% 
  mutate(ihs_bag_falle = ihs(bag_falle),
         ihs_bag_tode = ihs(bag_tode))
dta <- dta %>% 
  group_by(canton_codes) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(lag_ihs_bag_falle = dplyr::lag(ihs_bag_falle, n=1),
         lag_ihs_bag_tode = dplyr::lag(ihs_bag_tode, n=1),
         lag_bag_cum_falle = dplyr::lag(bag_cum_falle, n=1),
         lag_bag_cum_tode = dplyr::lag(bag_cum_tode, n=1),
         incr = if_else(lag_bag_cum_falle > 0, bag_falle / lag_bag_cum_falle, NA_real_), # incidence ratio
         dln_bag_cum_falle = if_else(lag_bag_cum_falle > 0, log(bag_cum_falle) - log(lag_bag_cum_falle), NA_real_) # log-diff.
  ) %>%
  ungroup()

# merge test data to data frame
dta <- full_join(dta, test_dta, c("date"="date"))

# merge cantonal population data to data frame
dta <- full_join(dta, canton_pop, c("canton_codes"="canton_codes"))

## save data
write_csv(dta, path=here::here("Data","Merged_panel_data.csv"))
write_dta(dta, path=here::here("Data","Merged_panel_data.dta"))
