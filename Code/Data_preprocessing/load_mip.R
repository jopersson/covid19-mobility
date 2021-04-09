library("strucchange")
library("dplyr")
library("tidyr")
library("readr")
library("lubridate")
library("stringr")
library("here")

rm(list=ls())

# load MIP trip data
mip <- read_csv(here::here("Data","MIP_data","MIP_trips.csv"))

# add canton codes
canton_names <- read_csv(here::here("Data","Canton_codes.csv")) %>% dplyr::select(-canton_names)
mip <- left_join(mip, canton_names, c("canton_name"="MIP_canton_names")) 

# shift 2019 date by one year
mip <- mip %>% 
  mutate(date_original = date, 
         date = if_else(date < as.Date("2019-12-01"),date + 364,date),
         wd_original = weekdays(date_original),
         wd = weekdays(date_original),
         sunday = if_else(wd == "Sunday",date,as.Date(NA)))

# to wide format: years as columns
mip_wide <- mip %>% 
  dplyr::select(date,wd,canton_name,canton_codes,total_trips,train_trips,highway_trips,road_trips,commuter_trips,non_commuter_trips,dataset,sunday) %>%
  pivot_wider(names_from="dataset",values_from=c("total_trips","train_trips","highway_trips","road_trips","commuter_trips","non_commuter_trips")) %>%
  mutate_at(vars(ends_with("2019"),ends_with("2020")),
            list(ln=~log(.))
            ) %>%
  rename_at( vars( contains( "_ln") ), list( ~paste("ln", gsub("_ln", "", .), sep = "_") ) )

# save
write_csv(mip_wide,path=here::here("Data","MIP_data","processed_MIP.csv"))
