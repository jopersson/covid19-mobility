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
library(readxl)
library(reshape2)
library(gridExtra)

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

# rename to canton codes
CH_df$NAME <- as.factor(CH_df$NAME)

levels(CH_df$NAME) <- list(AG  = "Aargau", BS = "Basel-Stadt", GL = "Glarus",
                           NE = "Neuchâtel", SZ = "Schwyz", TI = "Ticino",
                           ZH = "Zürich", AR = "Appenzell Ausserrhoden", BE = "Bern",
                           GR = "Graubünden", NW = "Nidwalden", SO = "Solothurn",
                           UR = "Uri", ZG = "Zug", AI = "Appenzell Innerrhoden",
                           FR = "Fribourg", JU = "Jura", OW = "Obwalden",
                           SG = "St. Gallen", VS = "Valais", BL = "Basel-Landschaft",
                           GE = "Genève", LU = "Luzern", SH = "Schaffhausen",
                           TG = "Thurgau", VD = "Vaud", NE = "NeuchÃ¢tel",
                           GR = "GraubÃ¼nden", ZH = "ZÃ¼rich", GE = "GenÃ¨ve")


###############################################################################
### Data pre-processing                                                     ###
###############################################################################

# read data
dta <- read_csv(here::here("Data","BFS_canton_population","canton_pop_2020_month.csv"))

colnames(dta) <- c("canton_codes", "Jan","Feb","Mar","Apr")
dta <- dta %>% select(canton_codes, Feb)

# long to wide format
dta <- spread(dta, key = canton_codes, value = Feb)

# initialize new columns
CH_df$Population <- -1


# add network activity data to canton polygon
for( canton in unique(CH_df$NAME)[0:26]){
  
  # get all row indices for canton
  row_idx = rownames(subset(CH_df, NAME == canton))
  
  # get cantonal population values
  value <- dta[as.character(canton)]
  
  # assign values to canton subset in data frame
  CH_df[row_idx,"Population"] <- value
}



###############################################################################
### SUI heatmap February                                                    ###
###############################################################################

# plot map1
map <-  ggplot(data = CH_df, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = Population)) +
  
  # plot polygons and borders
  geom_polygon(colour = "black", 
               size = 0.1, 
               aes(group = group)) +
  
  # activity fill colors
  scale_fill_gradient(low = "white", high = "#e03428",
                      limits=c(0,2000000),
                      breaks = c(0,500000,1000000,1500000,2000000),
                      labels = c("0","0.5","1","1,5","2")) +
  
  # theme parameters
  theme_classic() +
  
  # title
  labs(title = "",
       fill="Population size\n(x 1,000,000)") +
  
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
### Plot                                                                    ###
###############################################################################

# common legend for combined ggplots
legend <- get_legend(map)
map <- map + theme(legend.position="none")

blank <- grid.rect(gp=gpar(col="white"))

# show plots
map
plot_all <- grid.arrange(map, legend)


# write pdfs
ggsave(filename = "./Plots/population/heatplot_canton_populations_february.pdf", plot = plot_all, width = 7, height = 3.5)

