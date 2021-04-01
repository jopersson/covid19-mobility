library(tidyverse)
library(latex2exp)
library(reshape2)

rm(list=ls())

# read data
df_cases <- read.table(here::here("Data","BAG_covid","BAG_cases_per100k_by_canton_from_1st_case.csv"), header = TRUE, sep = ",")

###############################################################################
### data preparations                                                       ###
###############################################################################

# Rename column and change format
df_cases <- df_cases %>% 
  rename(Days = X) %>%
  mutate_at(vars(-Days), log2)

# x_end value
# x_end = max(df_cases$Days)
x_end = 70

# melt data
melt_df <- melt(df_cases,id="Days")

# number of rows and columns
n_rows <- nrow(melt_df)

# add line width and color columns to data frame
melt_df$size <- c(rep(0.2,n_rows))
melt_df$color <- c(rep('grey',n_rows))

# custom line width and color
melt_df$size[melt_df$variable=='GE'] <- 0.6
melt_df$color[melt_df$variable=='GE'] <- "#12868e"

melt_df$size[melt_df$variable=='ZH'] <- 0.6
melt_df$color[melt_df$variable=='ZH'] <- "#e5921a"

melt_df$size[melt_df$variable=='TI'] <- 0.6
melt_df$color[melt_df$variable=='TI'] <- "#884c6e"

# plot
plot <-  ggplot(data = melt_df, aes( x = Days, 
                                     y = value, 
                                      group = variable)) + 
  
    # layout specifications
    theme(
      # title, subtitle
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      
      # axes title
      axis.title.x = element_text(size = 8, vjust = -3, hjust = 0.5),
      axis.title.y = element_text(size = 8, vjust = 3, hjust = 0.5),
      
      # axes text
      axis.text.x = element_text(color = 'black', size = 6, angle = 0, hjust = 0.5, vjust = 0),
      axis.text.y = element_text(size = 6),
      
      # axes ticks 
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = 0.1),
      
      # axes lines
      axis.line.x = element_line(color="black", size = 0.2),
      axis.line.y = element_line(color="black", size = 0),
      
      # panel and margins
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(c(0.5,1,1,1), "line"),
      
      # legend 
      legend.position = "none") +

  # axes titles  
  xlab("Number of days since 1st reported case") +
  ylab("Cases per 100,000 inhabitants") + 
  
  #
  scale_x_continuous(limits = c(0,x_end),
                     breaks = seq(0,x_end,10),
                     expand = c(0,0)) +
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(0,10.05),
                     breaks = c(0,3.3219280949,6.6438561898,9.9657842847),
                     labels = c("1","10","100","1000"),
                     expand = c(0, 0))

# horizontal grid lines
for (i in c(0,3.3219280949,6.6438561898,9.9657842847)) plot <- plot + geom_segment(x=0, xend=x_end, y=i, yend=i, size=0.1, color="grey")

# vertical grid lines
for (j in seq(0,x_end,10)) plot <- plot + geom_segment(x=j, xend=j, y=0, yend=10, size=0.1, color="grey")

# annotate canton names (left-align)
plot <- plot + annotate("text",
                        fontface = "bold",
                        x = 12,
                        y = 0.5,
                        label = "Zurich",
                        color = "#e5921a",
                        hjust = 0,
                        size = 2.5)
  
# annotate canton names (right-align)
plot <- plot + annotate("text",
                        fontface = c("bold","bold"),
                        x = c(37, 24),
                        y = c(9.75,8.5),
                        label = c("Geneve","Ticino"),
                        color = c("#12868e","#884c6e"),
                        hjust = 1,
                        size = c(2.5,2.5))

# plot grey lines
df_grey <- subset(melt_df, color == 'grey')
plot <- plot + geom_line(data = df_grey, size = df_grey$size, color = df_grey$color)

# plot colored lines
df_color <- subset(melt_df, color != 'grey')
plot <- plot + geom_line(data = df_color, size = df_color$size, color = df_color$color)
plot

# save file
ggsave(filename = "./Plots/Fig_3/Growth_by_canton_from_1st_case.pdf", plot = plot, width = 3, height = 3)