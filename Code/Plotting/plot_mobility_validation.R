library(readxl)
library(here)
library(lattice)
library(ggplot)
library(ggpmisc)
library(ggpubr)
library(latticeExtra)
library(dplyr)
library(readr)
library(forcats)
library(gridExtra)

rm(list=ls())

source(here::here("Code","helper_functions.R"))

outpath <- here::here("Plots/mobility_validation/")


###############################################################################
### plot correlations between distance travelled in MIP and Intervista data ###
###############################################################################


# read data
dta_dist_IV <- read_csv(file=here::here("Data","Intervista_AG","distances_radii.csv"))
dta_dist_MIP <- read_csv(file=here::here("Data","MIP_data","Avg_travel_distances","Avg_daily_travel_distance_cantons.csv"))
dta_trips_MIP <- read_csv(file=here::here("Data","Merged_panel_data.csv"))


## Pre-processing ###############################################################################

# datetime to date
dta_dist_MIP$date <- as.Date(dta_dist_MIP$date)
dta_trips_MIP$date <- as.Date(dta_trips_MIP$date)


# keep only needed columns
dta_dist_IV <- dta_dist_IV %>% select(date, measure, type, mobility_total, mobility_car_yes, mobility_workschool, mobility_leisure, mobility_shopping, mobility_other_purpose, mobility_publictransport, mobility_other_mode)
dta_dist_MIP <- dta_dist_MIP %>% select(date, canton_codes, avg_distance_km)
dta_trips_MIP <- 
  dta_trips_MIP %>% select(date, canton_codes, total_trips_2020, train_trips_2020, highway_trips_2020, road_trips_2020, commuter_trips_2020, non_commuter_trips_2020)

# filter dates
dta_dist_IV <- dta_dist_IV %>% 
  filter(date >= as.Date("2020-02-10")) %>% 
  filter(date <= as.Date("2020-04-26")) 

dta_dist_IV <- dta_dist_IV %>% 
  filter(type == "mean") %>% 
  filter(measure == "distance") 

dta_trips_MIP <- dta_trips_MIP %>% 
  filter(date >= as.Date("2020-02-10")) %>% 
  filter(date <= as.Date("2020-04-26")) 


# get avg distance travelled in MIP data at the country level
dta_dist_MIP_ch <- dta_dist_MIP %>% 
  filter(canton_codes == "CH") %>% 
  select(date, avg_distance_km)

# get MIP trips on national level
dta_trips_MIP_ch <- aggregate(dta_trips_MIP[,3:8], by=list(dta_trips_MIP$date), sum)
colnames(dta_trips_MIP_ch)[1] <- "date"

## Join data ###############################################################################

# join MIP data on trips and avg distance at canton level
dta_dist_trips_cantons <- merge(dta_dist_MIP, dta_trips_MIP, by = c("canton_codes","date"))

# join MIP avg distance with Intervista avg distance data at national level
dta_dist_ch <- merge(dta_dist_MIP_ch, dta_dist_IV, by = "date") %>% select(-c("type","measure"))

# join MIP data on trips with Intervista avg distance data at national level
dta_dist_trips_ch <- merge(dta_trips_MIP_ch, dta_dist_IV, by = "date") %>% select(-c("type","measure"))

# scale variable for plot
dta_dist_trips_cantons[, 4:9] <- dta_dist_trips_cantons[, 4:9]/1000000
dta_dist_trips_ch[, 2:7] <- dta_dist_trips_ch[, 2:7]/1000000

# order cantons
dta_dist_trips_cantons$canton_codes <- as.factor(dta_dist_trips_cantons$canton_codes)

# combine leisure and shopping trips to create non-commuting trips from Intervista
dta_dist_trips_ch$mobility_leisure_shopping <- dta_dist_trips_ch$mobility_leisure + dta_dist_trips_ch$mobility_shopping


## Plot: Correlation avg. distance in MIP vs trips in MIP, by category (canton level) ######################################## 

# correlation between avg distance travelled and number of trips per day and canton (MIP data)
plot_total <- xyplot(avg_distance_km ~ total_trips_2020 | canton_codes, 
               data=dta_dist_trips_cantons,
               as.table = TRUE,
               ylab="Avg. daily distance travelled (km)", xlab="Number of daily trips (x 1,000,000)",
               col = "#d34f53",
               par.settings = list(superpose.symbol = list(col = "#d34f53"), strip.background=list(col="lightgrey")),
               auto.key = list(space = "inside", corner = c(1,0), cex.title = 1, title = "Trips", text = "Total"))
plot_total


plot_mode <- xyplot(avg_distance_km ~ highway_trips_2020 + road_trips_2020 + train_trips_2020 | canton_codes, 
               data=dta_dist_trips_cantons,
               as.table = TRUE,
               ylab="Avg. daily distance travelled (km)", xlab="Number of daily trips (x 1,000,000)",
               col = c("#884c6e","#73c2c0","#e5921a"),
               par.settings = list(superpose.symbol = list(col = c("#884c6e","#73c2c0","#e5921a")), strip.background=list(col="lightgrey")),
               auto.key = list(space = "inside", corner = c(1,0), cex.title = 1, title = "Mode", text = c("Highway","Road","Train")))
plot_mode


plot_purpose <- xyplot(avg_distance_km ~ commuter_trips_2020 + non_commuter_trips_2020 | canton_codes, 
               data=dta_dist_trips_cantons,
               as.table = TRUE,
               ylab="Avg. daily distance travelled (km)", xlab="Number of daily trips (x 1,000,000)",
               col = c("#008694","#0e5b90"),
               par.settings = list(superpose.symbol = list(col = c("#008694","#0e5b90")), strip.background=list(col="lightgrey")),
               auto.key = list(space = "inside", corner = c(1,0), cex.title = 1, title = "Purpose", text = c("Commuters","Non-commuters")))
plot_purpose


## Plot: Correlation avg. distance MIP vs Intervista (national level) ######################################## 

# correlation between avg distance travelled in MIP data and avg distance travelled in Intervista data

my.formula <- dta_dist_ch$mobility_total ~ dta_dist_ch$avg_distance_km
plot_dist <- ggplot(data = dta_dist_ch, aes(y = mobility_total, x = avg_distance_km)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(title="Avg. daily distance travelled (km)", y="Intervista data", x="MIP data") +
  geom_point(color = "#d34f53") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_dist


## Plot: Correlation trips in MIP vs avg. distance Intervista, by category (national level) ######################################## 

# correlation between number of trips in MIP data and avg distance travelled in Intervista data by different modes

my.formula <- dta_dist_trips_ch$mobility_total ~ dta_dist_trips_ch$total_trips_2020
plot_total_mobility <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_total, x = total_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) in total", x="Daily total trips (x1,000,000)") +
  geom_point(color = "#d34f53") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_total_mobility

my.formula <- dta_dist_trips_ch$mobility_car_yes ~ dta_dist_trips_ch$highway_trips_2020
plot_highway_car <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_car_yes, x = highway_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) by car", x="Daily highway trips (x1,000,000)") +
  geom_point(color = "#884c6e") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_highway_car

my.formula <- dta_dist_trips_ch$mobility_car_yes ~ dta_dist_trips_ch$road_trips_2020
plot_road_car <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_car_yes, x = road_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) by car", x="Daily road trips (x1,000,000)") +
  geom_point(color = "#73c2c0") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_road_car

my.formula <- dta_dist_trips_ch$mobility_publictransport ~ dta_dist_trips_ch$train_trips_2020
plot_train_public <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_publictransport, x = train_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) by public transport", x="Daily train trips (x1,000,000)") +
  geom_point(color = "#e5921a") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_train_public

my.formula <- dta_dist_trips_ch$mobility_workschool ~ dta_dist_trips_ch$commuter_trips_2020
plot_commute_school <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_workschool, x = commuter_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) for school or work", x="Daily commuting trips (x1,000,000)") +
  geom_point(color = "#008694") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_commute_school

my.formula <- dta_dist_trips_ch$mobility_leisure_shopping ~ dta_dist_trips_ch$non_commuter_trips_2020
plot_noncommute <- ggplot(data = dta_dist_trips_ch, aes(y = mobility_leisure_shopping, x = non_commuter_trips_2020)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  labs(y="Avg. daily distance (km) for shopping and leisure", x="Daily non-commuting trips (x1,000,000)") +
  geom_point(color = "#0e5b90") +
  guides(color = FALSE, size = FALSE) +
  theme_bw()
plot_noncommute


## Write plots to file ############################################################################


pdf(file = paste0(outpath, "trip_total_distance_correlations_MIP.pdf"), width = 8, height = 4)
  plot_total
dev.off()

pdf(file = paste0(outpath, "trip_mode_distance_correlations_MIP.pdf"), width = 8, height = 4)
  plot_mode
dev.off()

pdf(file = paste0(outpath, "trip_purpose_distance_correlations_MIP.pdf"), width = 8, height = 4)
  plot_purpose
dev.off()


pdf(file = paste0(outpath, "distance_correlations_MIP_Intervista.pdf"), width = 6, height = 4)
  plot_dist
dev.off()


pdf(file = paste0(outpath, "trips_MIP_vs_distance_Intervista_category.pdf"), width = 10, height = 6)
  grid.arrange( plot_highway_car, plot_road_car, plot_train_public,
                plot_commute_school, plot_noncommute, plot_total_mobility,
                nrow = 2)
dev.off()
