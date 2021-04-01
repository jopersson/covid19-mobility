
# write MIP data for average daily distance traveled (km)

rm(list=ls())

# load
avg_dist <- read.csv(here::here("Data","MIP_data","Avg_travel_distances","covid-19-tagliche-reiseaktivitat-in-der-schweiz-februar-mai-2020-de.csv"), sep=";")

# preprocess
colnames(avg_dist) = c("date", "canton_codes", "avg_distance_km")
avg_dist$date <- as.Date(avg_dist$date)
avg_dist <- arrange(avg_dist, canton_codes, date)

## write data
write_csv(avg_dist, path=here::here("Data","MIP_data","Avg_travel_distances","Avg_daily_travel_distance_cantons.csv"))