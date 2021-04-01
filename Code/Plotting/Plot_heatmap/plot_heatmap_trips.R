require(magrittr)
require(rgdal)
require(RColorBrewer)
require(gridExtra)
require(cowplot)
library(janitor)
require(grid)
require(tidyverse)
require(dplyr)
require(sp)
require(ggspatial)

rm(list=ls())

# load helper functions
source(here::here("Code","helper_functions.R"))


###############################################################################
### Shape file pre-processing                                               ###
###############################################################################

# load CH shape file
CH <- readOGR(here::here("CH_shapefile","BOUNDARIES_2020","DATEN","swissBOUNDARIES3D","SHAPEFILE_LV03_LN02","swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp"))

# fortify data
CH_df <- fortify(CH)

# allocate an id variable to the shape data
CH$id <- row.names(CH) 

# join the data
CH_df <- left_join(CH_df, CH@data) 

# fix canton names reading errors due to non-english letters
CH_df <- CH_df %>% 
  mutate(NAME = replace(NAME, NAME == "GraubÃ¼nden", "Graubünden")) %>% 
  mutate(NAME = replace(NAME, NAME == "ZÃ¼rich", "Zürich")) %>% 
  mutate(NAME = replace(NAME, NAME == "NeuchÃ¢tel", "Neuchâtel")) %>% 
  mutate(NAME = replace(NAME, NAME == "GenÃ¨ve", "Genève"))

###############################################################################
### Data pre-processing                                                     ###
###############################################################################

# read data
dta19 <- read_csv(file=here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2019.csv"))
dta20 <- read_csv(file=here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2020.csv"))

# mutate data19
dta19 <- dta19 %>% mutate(X1 = as.Date(X1)) %>% 
  rename(Date = X1)

# mutate data20
dta20 <- dta20 %>% mutate(X1 = as.Date(X1)) %>% 
  rename(Date = X1)

# change column names to Cadastre lingo
colnames(dta19) <- c("Date", "Solothurn", "Basel-Landschaft", "Schwyz", "Aargau", "Ticino", "Schaffhausen", "Uri", "Fribourg", "Luzern", "Genève", "Zug", "Jura", "Obwalden", "Thurgau", "Glarus", "Basel-Stadt", "Appenzell Ausserrhoden", "Nidwalden", "Zürich", "Appenzell Innerrhoden", "Neuchâtel", "Bern", "Graubünden", "St. Gallen", "Vaud", "Valais")
colnames(dta20) <- c("Date", "Thurgau", "Glarus", "Zug", "Jura", "Obwalden", "Appenzell Ausserrhoden", "Nidwalden", "Zürich", "Vaud", "Basel-Stadt", "Valais", "Neuchâtel", "Appenzell Innerrhoden", "Solothurn", "Aargau", "Schwyz", "Uri", "Schaffhausen", "Ticino", "Basel-Landschaft", "Genève", "Luzern", "Fribourg", "Bern", "Graubünden", "St. Gallen")

# start and end date
start_date19 <- as.Date("2019-03-25")
end_date19 <- as.Date("2019-04-01")

start_date20 <- as.Date("2020-03-25")
end_date20 <- as.Date("2020-04-01")

# filter data frame on dates
dta19 <- dta19 %>% filter(Date >= start_date19 & Date < end_date19)
dta20 <- dta20 %>% filter(Date >= start_date20 & Date < end_date20)

# total per canton as new row
dta19 <- dta19 %>%
  adorn_totals("row")
dta20 <- dta20 %>%
  adorn_totals("row")

# initialize new columns
CH_df$Mobility19 <- -1
CH_df$Mobility20 <- -1

# add network activity data to canton polygon
for( canton in unique(CH_df$NAME)[0:26]){
  
  # get all row indices for canton
  row_idx = rownames(subset(CH_df, NAME == canton))
  
  # get cantonal values from MIP data
  value19 <- subset(dta19, Date == "Total")[as.character(canton)]
  value20 <- subset(dta20, Date == "Total")[as.character(canton)]
  
  # assign values to canton subset in data frame
  CH_df[row_idx,"Mobility19"] <- value19
  CH_df[row_idx,"Mobility20"] <- value20
}

###############################################################################
### SUI heatmap 2019                                                        ###
###############################################################################

# plot map1
CH_map1 <-  ggplot(data = CH_df, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = Mobility19)) +
  
  # plot polygons and borders
  geom_polygon(colour = "black", 
               size = 0.1, 
               aes(group = group)) +
  
  # activity fill colors
  scale_fill_gradient(low = "white", high = "#0e5b90",
                      limits=c(0,30000000),
                      breaks = c(0,5000000,10000000,15000000,20000000,25000000,30000000),
                      labels = c("0","5","10","15","20","25","30")) +
  
  # theme parameters
  theme_classic() +
  
  # title
  labs(title = "",
       fill="Trip count\n(x 1,000,000)") +
  
  theme(
    # title, subtitle
    plot.title=element_text(size = 10, face = "bold", hjust = 0.1, vjust = -5, family="sans"),
    plot.subtitle=element_blank(),
    
    # axes titles and text
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    
    # background specification and margins
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    
    # legend specifications
    legend.position="bottom",
    
    legend.title = element_text(size=12),
    legend.text=element_text(size=12),
    # legend.direction = "vertical",
    # legend.key.width = unit(.5, "cm"),
    # legend.key.height = unit(1.25, "cm")) +
    legend.direction = "horizontal",
    legend.key.width = unit(1.25, "cm"),
    legend.key.height = unit(.25, "cm")) +
  
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                title.vjust = 0.5,
                                label.position = "bottom")) +
  
  # fix ratio   
  coord_fixed()

###############################################################################
### SUI heatmap 2020                                                        ###
###############################################################################

# plot map2
CH_map2 <-  ggplot(data = CH_df, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = Mobility20)) +
  
  # plot polygons and borders
  geom_polygon(colour = "black", 
               size = 0.1, 
               aes(group = group)) +
  
  # activity fill colors
  scale_fill_gradient(low = "white", high = "#0e5b90",
                      limits=c(0,30000000),
                      breaks = c(0,5000000,10000000,15000000,20000000,25000000,30000000),
                      labels = c("0","5","10","15","20","25","30")) +
  
  # theme parameters
  theme_classic() +
  
  # title
  labs(title = "") +
  
  theme(
    # title, subtitle
    plot.title=element_text(size = 10, face = "bold", hjust = 0.1, vjust = -5, family="sans"),
    plot.subtitle=element_blank(),
    
    # axes titles and text
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    
    # background specification
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    
    # legend specification
    legend.position = "none") +
  
  # fix ratio
  coord_fixed()


# common legend for combined ggplots
legend <- get_legend(CH_map1)
CH_map1 <- CH_map1 + theme(legend.position="none")
CH_map2 <- CH_map2 + theme(legend.position="none")

# show plots
CH_map1
CH_map2
legend

# save plots
ggsave("./Plots/Fig_1/CH_heatmap_month2019.pdf", plot = CH_map1, width = 4, height = 4)
ggsave("./Plots/Fig_1/CH_heatmap_month2020.pdf", plot = CH_map2, width = 4, height = 4)
ggsave("./Plots/Fig_1/CH_heatmap_colorbar_horizontal.pdf", plot = legend, width = 4, height = 1)
