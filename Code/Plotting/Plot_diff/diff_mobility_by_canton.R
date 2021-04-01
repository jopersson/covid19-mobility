require(reshape2)
require(ggplot2)
require(scales)
require(readr)
require(grid)
require(magrittr)

rm(list=ls())


###############################################################################
### diff plot mobility by canton                                            ###
###############################################################################

# read data
dta19 <- read_csv(file=here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2019.csv"))
dta20 <- read_csv(file=here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2020.csv"))

# mutate data '19
dta19 <- dta19 %>% mutate(X1 = as.Date(X1)) %>% 
                   rename(Date = X1)

# mutate data '20
dta20 <- dta20 %>% mutate(X1 = as.Date(X1)) %>% 
                   rename(Date = X1)

# duplicate data frame for log difference
df_perc_ch1920 <- dta20

# calculate percentage change mobility 20 vs. 19
for(col in colnames(df_perc_ch1920)[2:27]){
  df_perc_ch1920[,col] <- dta20[,col]/dta19[,col]
}

# start and end data
start_date <- as.Date("2020-02-10")
end_date <- as.Date("2020-04-26")

# filter data frame on dates
df_perc_ch1920 <- df_perc_ch1920 %>% filter(Date >= start_date & Date <= end_date)

# number of rows and columns
n_rows <- nrow(df_perc_ch1920)
n_cols <- ncol(df_perc_ch1920)

# weekend days
sats <- which(weekdays(df_perc_ch1920$Date) == "Saturday")
suns <- which(weekdays(df_perc_ch1920$Date) == "Sunday")

# melt data
melt_df <- melt(df_perc_ch1920,id="Date")

# add line width and color columns to data frame
melt_df$size <- c(rep(0.1,(n_cols-1)*n_rows))
melt_df$color <- c(rep('grey',(n_cols-1)*n_rows))

# custom line width and color
melt_df$size[melt_df$variable=='Zurich'] <- 0.6
melt_df$color[melt_df$variable=='Zurich'] <- "#e5921a"

melt_df$size[melt_df$variable=='Geneva'] <- 0.6
melt_df$color[melt_df$variable=='Geneva'] <- "#12868e"

melt_df$size[melt_df$variable=='Ticino'] <- 0.6
melt_df$color[melt_df$variable=='Ticino'] <- "#884c6e"


# plot 
diff_plot <-  ggplot(melt_df, aes( x = Date,
                                   y = value,
                                   group = variable)) + 
  
  # lines colors
  scale_colour_manual(values = rep('grey',26)) +
  
  # layout specifications
  theme(
    # title, subtitle
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    # axes title
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8, vjust = 4, hjust = 0.365),
    
    # axes text
    axis.text.x = element_text(color = 'black', size = 7, angle = 0, hjust = 0.5, vjust = 0),
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
    plot.margin = unit(c(0.5,0.5,0.25,0.75), "line"),
    
    # legend 
    legend.position = "none") +
  
  # axes title
  ylab("Difference in mobility 2020 vs. 2019") + 
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(0.2,1.97),
                     breaks = seq(0.2,2,0.2),
                     labels = c("-80%","-60%","-40%","-20%","0%","+20%","+40%","+60%","+80%",""),
                     expand = c(0, 0)) +
  
  scale_x_date(breaks = "week",
               labels = date_format("%d\n%b"),
               limits = as.Date(c("2020-02-10","2020-04-27")),
               expand = c(0,0)) +
  
  coord_cartesian(clip = "off") +
  
  # annotate canton names (left-align)
  annotate("text",
           fontface = c("bold","bold","bold","plain"),
           x = as.Date(c("2020-02-17","2020-03-02","2020-04-15","2020-02-29")),
           y = c(1.25,0.75,1.25,1.63),
           label = c("Geneva","Ticino","Zurich","Neuchatel"),
           color = c("#12868e","#884c6e",'#e5921a','grey'),
           hjust = 0,
           size = c(2.5,2.5,2.5,1.5)) +
  
  # annotate canton names (right-align)
  annotate("text",
           fontface = "plain",
           x = c(as.Date("2020-03-07"),as.Date("2020-04-10")+0.5),
           y = c(1.73,1.12),
           label = "Appenzell\nInnerrhoden",
           color = 'grey',
           hjust = 1,
           size = 1.5) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-02-25"),
               xend = as.Date("2020-02-25"),
               y = 0.2,
               yend = 1.83,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-02-28"),
               xend = as.Date("2020-02-28"),
               y = 0.2,
               yend = 1.89,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-13"),
               xend = as.Date("2020-03-13"),
               y = 0.2,
               yend = 1.86,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-16"),
               xend = as.Date("2020-03-16"),
               y = 0.2,
               yend = 1.94,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  # annotate event description (right-aligned)
  annotate("text",
           fontface = "plain",
           color = 'grey25',
           x = c(as.Date("2020-02-25")-0.75,
                 as.Date("2020-02-28")-0.5,
                 as.Date("2020-03-13")-0.75,
                 as.Date("2020-03-16")-0.5),
           y = c(1.84,
                 1.91,
                 1.87,
                 1.97),
           label = c("First case",
                     "Ban>1000",
                     "Ban>100",
                     "Closed schools"),
           hjust = 1,
           size = 2.5) +

  # draw vertical lines
  geom_segment(x = as.Date("2020-03-18"),
               xend = as.Date("2020-03-18"),
               y = 0.2,
               yend = 1.94,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-20"),
               xend = as.Date("2020-03-20"),
               y = 0.2,
               yend = 1.88,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-25"),
               xend = as.Date("2020-03-25"),
               y = 0.2,
               yend = 1.83,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  # geom_segment(x = as.Date("2020-03-24"),
  #              xend = as.Date("2020-03-24"),
  #              y = 0.2,
  #              yend = 1.83,
  #              size = 0.1,
  #              color = 'grey',
  #              linetype = 'dashed') +

  # annotate event description (left-aligned)
  annotate("text",
           fontface = "plain",
           color = 'grey25',
           x = c(as.Date("2020-03-18")+0.5,
                 as.Date("2020-03-20")+0.75,
                 as.Date("2020-03-25")+0.75),
           y = c(1.97,
                 1.89,
                 1.84),
           label = c("Closed venues",
                     "Ban>5",
                     "Closed borders"),
           hjust = 0,
           size = 2.5) +

  # annotate weekend description
  annotate("text",
           fontface = c("bold",
                        "bold",
                        "plain"),
           color = c('grey',
                     'grey',
                     'grey25'),
           x = c(as.Date("2020-04-12"),
                 as.Date("2020-04-19"),
                 as.Date("2020-04-26")-0.5),
           y = c(1.725,
                 1.725,
                 1.84),
           label = c("Eastern\n2020",
                     "Eastern\n2019",
                     "Weekend"),
           hjust = 1,
           size = c(2,
                    2,
                    2.5)) +
  
  # draw vertical line (weekend)
  geom_segment(x = as.Date("2020-04-26"),
               xend = as.Date("2020-04-26"),
               y = 1.76,
               yend = 1.83,
               size = 0.2,
               color = 'grey',
               linetype = 'solid') +

  # circle on dashed vertical lines
  annotate("point",
           size = 0.3,
           shape = 19,
           color = 'grey',
           x = as.Date(c("2020-02-25","2020-02-28","2020-03-13","2020-03-16","2020-03-18","2020-03-20","2020-03-25","2020-04-26")),
           y = c(1.83,1.89,1.86,1.94,1.94,1.88,1.83,1.83))


# horizontal grid lines
diff_plot <- diff_plot + lapply(seq(0.2,1.8,0.2), function(i) { geom_segment(aes(x=start_date, 
                                                                                 xend=end_date+.5, 
                                                                                 y=i, 
                                                                                 yend=i), 
                                                                             size=0.1, 
                                                                             color="grey")})
# shaded weekend areas
diff_plot <- diff_plot + lapply(seq(1,11,1), function(i) {geom_rect(data=melt_df[1,],
                                                                    aes(xmin=melt_df[sats[i],1]+.5, xmax=melt_df[suns[i],1]+.5,
                                                                        ymin=0.2, ymax=1.8),
                                                                    color=NA, fill='grey', alpha=.3)})

# shade Eastern Monday 2020
diff_plot <- diff_plot + geom_rect(data=melt_df[1,],
                                   aes(xmin=as.Date("2020-04-12")+.5, xmax=as.Date("2020-04-13")+.5,
                                       ymin=0.2, ymax=1.8),
                                   color=NA, fill='grey', alpha=.3)

# shade Eastern Monday 2020
diff_plot <- diff_plot + geom_rect(data=melt_df[1,],
                                   aes(xmin=as.Date("2020-04-19")+.5, xmax=as.Date("2020-04-20")+.5,
                                       ymin=0.2, ymax=1.8),
                                   color=NA, fill='grey', alpha=.3)

# plot grey lines
df_grey <- subset(melt_df, color == 'grey')
diff_plot <- diff_plot + geom_line(data = df_grey, 
                                   size = df_grey$size, 
                                   color = df_grey$color)

# plot colored lines
df_color <- subset(melt_df, color != 'grey')
diff_plot <- diff_plot + geom_line(data = df_color, 
                                   size = df_color$size, 
                                   color = df_color$color)

# final plot
diff_plot

# save plot
ggsave(filename = "./Plots/Fig_1/diff_mobility_by_canton.pdf", plot = diff_plot, width = 6, height = 4)