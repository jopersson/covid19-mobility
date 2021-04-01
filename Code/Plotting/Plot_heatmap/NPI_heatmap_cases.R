library(tidyverse)
library(ggrepel)
library(reshape2)
library(ggplot2)

rm(list=ls())

## Data
df_start_NPI <- read_csv(here::here("Data","BAG_covid","BAG_cases_per100k_by_starting_date_NPI.csv"))

# rename column
df_start_NPI <- df_start_NPI %>% 
  rename(Canton = X1) 

# melt data frame
df_start_NPI_long <- df_start_NPI %>% pivot_longer(-Canton, names_to = "NPI", values_to = "Cases")

# specify order of plotting
df_start_NPI_long$NPI <- factor(df_start_NPI_long$NPI, 
                                levels = c("ban_1000","ban_100","closed_schools","closed_stores-bars","ban_5","closed_borders"))
# remove "ban > 1000"
df_start_NPI_long <- df_start_NPI_long  %>% 
  filter(NPI != "ban_1000")


# heatmap: cases till starting date NPI
heatmap_tiles <- ggplot(data = df_start_NPI_long, 
                        aes(x=NPI, y = reorder(Canton, desc(Canton)), fill=Cases)) +
                    
                  # tiles
                  geom_tile(aes(width=0.9, height=0.9)) + 
                    
                  # color gradient
                  scale_fill_gradient(low = "white", high = "#1d60a2", 
                                      limits=c(0,360), 
                                      breaks = c(0,50,100,150,200,250,300,350),
                                      labels = c('0','50','100','150','200','250','300','350')) + 
                    
                  # background colors
                  theme(panel.background = element_rect(fill = 'white', colour = 'white')) + 
                    
                  # axis labels
                  scale_x_discrete(labels=c("ban_100" = "\nBan > 100",
                                            "closed_schools" = "\nClosed\nschools", "closed_stores-bars" = "\nClosed\nvenues", 
                                            "ban_5" = "\nBan > 5", "closed_borders" = "\nClosed\nborders")) +
  
                  # # reverse plotting order y-axis
                  # scale_y_continuous(trans = "reverse") +
                    
                  # axes title position
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        text = element_text(size=8),
                        axis.text.x = element_text(angle=0, hjust=0.5, color = 'black'),
                        legend.key.height = unit(1, "cm"),
                        legend.title = element_text(size=8)) + 

                  labs(fill = "Cases per\n100,000\ninhabitants")

# save plot
heatmap_tiles
ggsave(filename = "./Plots/Fig_3/NPI_heatmap_cases.pdf", plot = heatmap_tiles, width = 5, height = 3.25)