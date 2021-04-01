
library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("lubridate")
library("here")
library("readxl")

rm(list=ls())

###############################################################################
### load BAG data                                                           ###
###############################################################################

## load all files and append
bag_version <- "downloaded_2020-05-20"
all_bag_files <- dir(here::here("Data","BAG_covid",bag_version))
bag <- list()
for (f in all_bag_files) {
  bag[[f]] <- read.table(here::here("Data","BAG_covid",bag_version,f),header=TRUE,sep="\t",quote="",fileEncoding="UTF-16LE",stringsAsFactors =FALSE,
                         colClasses = "character")
}
bag <- lapply(bag,as_tibble)
bag <- bind_rows(bag)
bag <- bag %>% janitor::clean_names()

# aggregate to get cases and deaths by canton and day
bag <- bag %>% 
  filter(fall_dt != "" | pttoddat!="") %>%
  mutate(date = paste0(fall_dt,pttoddat),
          date = dmy(date))
bag <- bag %>%
        mutate(
         anzahl_todesfalle=as.numeric(anzahl_todesfalle),
         anzahl_laborbestatigte_falle = as.numeric(anzahl_laborbestatigte_falle))  
bag <- bag %>%
  group_by(kanton,date) %>%
  summarise(bag_falle =sum(anzahl_laborbestatigte_falle),
            bag_tode  = sum(anzahl_todesfalle)) %>%
  ungroup()

# fill missing days
bag <- bag %>%
  group_by(kanton) %>%
  complete(date = seq(min(bag$date),max(bag$date),by=1)) %>%
  ungroup()  
bag <-   bag %>% replace_na(list(bag_falle=0,bag_tode=0))

# generate cumulated variables
bag <- bag %>%
  group_by(kanton) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(bag_cum_falle = cumsum(bag_falle),
         bag_cum_tode = cumsum(bag_tode)) %>%
  ungroup()

# remove liechtenstein      
bag <- bag %>% filter(kanton != "FL")

# rename date variable
bag <- bag %>%
  rename(date=date)

###############################################################################
### hospitalizations                                                        ###
###############################################################################


## load data on hospitalizations
bag_hosp  <- read_csv(here::here("Data","BAG_covid","bag_covid_19_data_csv_23_March_2021", "data", "COVID19Hosp_geoRegion.csv"))

# filter and rename columns
bag_hosp <- bag_hosp %>%
  select(geoRegion, datum, entries, sumTotal)

colnames(bag_hosp) <- c("kanton","date","bag_hosp","bag_cum_hosp")

# filter dates
start_date <- unique(bag$date)[1]
end_date <- unique(bag$date)[length(unique(bag$date))]

bag_hosp <- bag_hosp %>% 
  filter(date >= start_date) %>% 
  filter(date <= end_date) 

# filter out non-canton data
bag_hosp <- bag_hosp %>% 
  filter(kanton != "CH") %>% 
  filter(kanton != "CHFL") %>% 
  filter(kanton != "FL")

# join data
bag <- merge(bag, bag_hosp, by = c("kanton","date"))


###############################################################################
### find day of first case                                                  ###
###############################################################################

# find day of first case
bag <- bag %>% 
  mutate(date_t0 = if_else(bag_cum_falle >= 1, date, as.Date(NA))) %>%
  group_by(kanton) %>%
  mutate(date_t0 = min(date_t0, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(t = date - date_t0, t= as.integer(t) + 1)

# add canton names
canton_codes <- read_csv(here::here("Data","Canton_codes.csv"))
bag <- left_join(bag, canton_codes %>% dplyr::select(-MIP_canton_names)
                 ,c("kanton"="canton_codes"))  
bag <- bag %>% rename(canton_code = kanton)
bag <- bag %>% dplyr::select(date, canton_code, canton_names, everything() )

## saving
write_csv(bag, path=here::here("Data","BAG_covid","BAG_covid.csv"))

