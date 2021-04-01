require(reshape2)
require(ggplot2)
require(scales)
require(grid)
require(dplyr)
require(readr)

rm(list=ls())

###############################################################################
### ratio plot by reason of mobility 2019                                   ###
###############################################################################

# read data
dta_c20 <- read_csv(file=here::here("Data","MIP_data","Commuter_trips_ColumnF","commuter_trips_canton_per_day_2020.csv"))
dta_nc20 <- read_csv(file=here::here("Data","MIP_data","Non_commuter_trips_ColumnG","non_commuter_trips_canton_per_day_2020.csv"))
dta_tot20 <- read_csv(file=here::here("Data","MIP_data","Total_trip_counts_ColumnB","total_trip_count_canton_per_day_2020.csv"))

# mutate data
dta_c20 <- dta_c20 %>% mutate(X1 = as.Date(X1)) %>% rename(Date = X1)
dta_nc20 <- dta_nc20 %>% mutate(X1 = as.Date(X1)) %>% rename(Date = X1)
dta_tot20 <- dta_tot20 %>% mutate(X1 = as.Date(X1)) %>% rename(Date = X1)

# group by date and sum over all cantons to get nationwide mobility
cantons_c20 <- colnames(dta_c20[,2:27])
cantons_nc20 <- colnames(dta_nc20[,2:27])
cantons_tot20 <- colnames(dta_tot20[,2:27])

dta_c20$commuter <- rowSums(dta_c20[cantons_c20])
dta_nc20$non_commuter <- rowSums(dta_nc20[cantons_nc20])
dta_tot20$total <- rowSums(dta_tot20[cantons_tot20])

# new dataframes with share of trips by reason
df_20 <- as.data.frame(dta_c20$Date)

df_20$ratio_commuter_2020     <- dta_c20$commuter / dta_tot20$total
df_20$ratio_non_commuter_2020 <- dta_nc20$non_commuter / dta_tot20$total

# drop non-ratio columns
colnames(df_20) <- c("Date", "Commuter trips", "Non-commuter trips")

# start, end data
start_date <- as.Date("2020-02-10")
end_date <- as.Date("2020-04-26")

# filter data frame on dates
df_20 <- df_20 %>% filter(Date >= start_date & Date <= end_date)
n_rows <- nrow(df_20)
n_cols <- ncol(df_20)

# weekend days
sats <- which(weekdays(df_20$Date) == "Saturday")
suns <- which(weekdays(df_20$Date) == "Sunday")

# melt data
df_20_long <- melt(df_20,id="Date")
df_20_long$size <- c(rep(0.8,(n_rows*(n_cols-1))))


# colors
colors2 <- c("#008694","#0e5b90")

# plot 
diff_plot <-  ggplot(df_20_long, aes( x = Date,
                                        y = value,
                                        color = variable,
                                        group = variable)) + 
  
  # line colors
  scale_colour_manual(values = colors2) +
  
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
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(0.5,0.5,0.25,0.75), "line"),
    
    # legend
    legend.position = "none") +
  
  ylab("Share of total trips") +
  
  # x-axis range
  scale_x_date(breaks = "week",
               labels = date_format("%d\n%b"),
               limits = as.Date(c("2020-02-10","2020-04-27")),
               expand = c(0,0)) +
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(0,1.1),
                     breaks = seq(0, 1, 0.2),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     expand = c(0, 0)) +
  
  coord_cartesian(clip = "off") +
  
  # circle plot (by type)
  annotate("point",
           size = 0.1,
           shape = 19,
           color = colors2,
           x = start_date,
           y = c(df_20[1,2],df_20[1,3])) +
  
  # variable text (by reason)
  annotate("text",
           fontface = "bold",
           x = df_20_long[3,1],
           y = c(df_20[1,2]+0.01, df_20[1,3]-0.01),
           label = c("Commuters","Non-commuters"),
           color = colors2,
           hjust = 0,
           size = 2.5) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-02-25"),
               xend = as.Date("2020-02-25"),
               y = 0,
               yend = 1.025,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-02-28"),
               xend = as.Date("2020-02-28"),
               y = 0,
               yend = 1.1,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-13"),
               xend = as.Date("2020-03-13"),
               y = 0,
               yend = 1.025,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-16"),
               xend = as.Date("2020-03-16"),
               y = 0,
               yend = 1.1,
               size = 0.2,
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
           y = c(1.025,1.1,1.025,1.1),
           label = c("First case",
                     "Ban>1000",
                     "Ban>100",
                     "Closed schools"),
           hjust = 1,
           size = 2.5) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-03-17"),
               xend = as.Date("2020-03-17"),
               y = 0,
               yend = 1.1,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-20"),
               xend = as.Date("2020-03-20"),
               y = 0,
               yend = 1.06,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-25"),
               xend = as.Date("2020-03-25"),
               y = 0,
               yend = 1.025,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  # geom_segment(x = as.Date("2020-03-24"),
  #              xend = as.Date("2020-03-24"),
  #              y = 0,
  #              yend = 31000000,
  #              size = 0.2,
  #              color = 'grey',
  #              linetype = 'dashed') +
  
  # annotate event description (left-aligned)
  annotate("text",
           fontface = "plain",
           color = 'grey25',
           x = c(as.Date("2020-03-17")+0.75,
                 as.Date("2020-03-20")+0.75,
                 as.Date("2020-03-25")+0.75),
           y = c(1.1,
                 1.06,
                 1.025),
           label = c("Closed venues",
                     "Ban>5",
                     "Closed borders"),
           hjust = 0,
           size = 2.5) +
  
  # annotate("text",
  #          fontface = "plain",
  #          color = 'grey25',
  #          x = as.Date("2020-03-24")+0.5,
  #          y = 31500000,
  #          label = "Lockdown",
  #          hjust = 0,
  #          size = 2.5) +
  
  # draw vertical line (weekend)
  geom_segment(x = as.Date("2020-04-26")-0.5,
             xend = as.Date("2020-04-26")-0.5,
             y = 0.975,
             yend = 1.025,
             size = 0.2,
             color = 'grey',
             linetype = 'solid') +
  
  # annotate weekend and Eastern description
  annotate("text",
           fontface = c("plain","bold"),
           color = c('grey25','grey'),
           x = c(as.Date("2020-04-26")-1,as.Date("2020-04-13")),
           y = c(1.025,0.975),
           label = c("Weekend","Eastern"),
           hjust = 1,
           size = c(2.5,2)) +
  
  # bullet on dashed vertical lines
  annotate("point",
           size = 1,
           shape = 19,
           color = 'grey',
           x = as.Date(c("2020-02-25","2020-02-28","2020-03-13","2020-03-16","2020-03-17","2020-03-20","2020-03-25")),
           y = c(1.025, 1.1, 1.025, 1.1, 1.1, 1.06, 1.025))

# horizontal grid lines
diff_plot <- diff_plot + lapply(c(0,0.2,0.4,0.6,0.8,1), function(i) { geom_segment(aes(x=start_date, 
                                                                                                xend=end_date+.5, 
                                                                                                y=i, 
                                                                                                yend=i), 
                                                                                            size=0.1, 
                                                                                            color="grey")})

# shaded weekend areas
diff_plot <- diff_plot + lapply(seq(1,11,1), function(i) {geom_rect(data=df_20_long[1,],
                                                                    aes(xmin=df_20[sats[i],1], xmax=df_20[suns[i],1],
                                                                        ymin=0, ymax=1),
                                                                    color=NA, fill='grey', alpha=.3)})

# shade Eastern Monday
diff_plot <- diff_plot + geom_rect(data=df_20_long[1,],
                                   aes(xmin=as.Date("2020-04-12"), xmax=as.Date("2020-04-13"),
                                       ymin=0, ymax=1),
                                   color=NA, fill='grey', alpha=.3)

# plot lines on top of everything
diff_plot <- diff_plot + geom_line(aes(group=variable), size = df_20_long$size)

# final plot
diff_plot

# save plot
ggsave(filename = "./Plots/parallel_trends/ratio_mobility_by_reason_2020.pdf", plot = diff_plot, width = 8, height = 3, bg = "transparent")