library("strucchange")
library("dplyr")
library("tidyr")
library("readr")
library("lubridate")
library("ggplot2")
library("stringr")
library("here")

rm(list=ls())

# load MIP data
mip2019 <- read_csv(here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2019.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="total_trips") %>%
  mutate(dataset = "2019")
mip2020 <- read_csv(here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2020.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="total_trips") %>%
  mutate(dataset = "2020")
mip2019_train <- read_csv(here::here("Data","MIP_data","Train_trips_ColumnC","train_trips_canton_per_day_2019.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="train_trips") 
mip2020_train <- read_csv(here::here("Data","MIP_data","Train_trips_ColumnC","train_trips_canton_per_day_2020.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="train_trips")
 mip2019_road <- read_csv(here::here("Data","MIP_data","Road_trips_ColumnD","road_trips_canton_per_day_2019.csv")) %>%
   rename(date = X1) %>%  
   mutate(date = ymd(as.character(date))) %>%
   pivot_longer(-date,names_to="canton_name",values_to="road_trips") 
mip2020_road <- read_csv(here::here("Data","MIP_data","Road_trips_ColumnD","road_trips_canton_per_day_2020.csv")) %>%
   rename(date = X1) %>%  
   mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="road_trips") 
mip2019_highway <- read_csv(here::here("Data","MIP_data","Highway_trips_ColumnE","highway_trips_canton_per_day_2019.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="highway_trips") 
mip2020_highway <- read_csv(here::here("Data","MIP_data","Highway_trips_ColumnE","highway_trips_canton_per_day_2020.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="highway_trips")
mip2019_commuters <- read_csv(here::here("Data","MIP_data","Commuter_trips_ColumnF","commuter_trips_canton_per_day_2019.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="commuter_trips") 
mip2020_commuters <- read_csv(here::here("Data","MIP_data","Commuter_trips_ColumnF","commuter_trips_canton_per_day_2020.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="commuter_trips")
mip2019_non_commuters <- read_csv(here::here("Data","MIP_data","Non_commuter_trips_ColumnG","non_commuter_trips_canton_per_day_2019.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="non_commuter_trips") 
mip2020_non_commuters <- read_csv(here::here("Data","MIP_data","Non_commuter_trips_ColumnG","non_commuter_trips_canton_per_day_2020.csv")) %>%
  rename(date = X1) %>%  
  mutate(date = ymd(as.character(date))) %>%
  pivot_longer(-date,names_to="canton_name",values_to="non_commuter_trips")

# bind data
mip <- bind_rows(mip2019,mip2020)
mip_train <- bind_rows(mip2019_train,mip2020_train) %>% 
          mutate(canton_name = if_else(canton_name=="Appenzell-Auserrhoden","Appenzell-Ausserrhoden",canton_name))
mip_road <- bind_rows(mip2019_road,mip2020_road)
mip_highway <- bind_rows(mip2019_highway,mip2020_highway) %>% 
          mutate(canton_name = if_else(canton_name=="Appenzell-Auserrhoden","Appenzell-Ausserrhoden",canton_name))
mip_commuters <- bind_rows(mip2019_commuters,mip2020_commuters)  %>% 
          mutate(canton_name = if_else(canton_name=="Geneva","Geneve",canton_name))
mip_non_commuters <- bind_rows(mip2019_non_commuters,mip2020_non_commuters)

# merge data
mip <- full_join(mip, mip_train, by=c("date","canton_name"))
mip <- full_join(mip, mip_road, by=c("date","canton_name"))
mip <- full_join(mip, mip_highway, by=c("date","canton_name"))
mip <- full_join(mip, mip_commuters, by=c("date","canton_name"))
mip <- full_join(mip, mip_non_commuters, by=c("date","canton_name"))

# add canton codes
canton_names <- read_csv(here::here("Data","Canton_codes.csv")) %>% dplyr::select(-canton_names)
mip <- left_join(mip,canton_names,c("canton_name"="MIP_canton_names")) 

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
write_csv(mip,path=here::here("Data","MIP_data","processed_MIP_long.csv"))
write_csv(mip_wide,path=here::here("Data","MIP_data","processed_MIP_wide.csv"))
