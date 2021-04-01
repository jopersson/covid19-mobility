require(reshape2)
require(ggplot2)
require(scales)
require(grid)
require(dplyr)
require(readr)

rm(list=ls())


###############################################################################
### diff plot by reason  of mobility                                        ###
###############################################################################

# read data
dta <- read_csv(file=here::here("Data","Merged_panel_data.csv"))

# group by date and sum over all cantons to get nationwide mobility
col_names_by_reason <- c("total_trips_2020","commuter_trips_2020","non_commuter_trips_2020")
df_sum <- aggregate(dta[col_names_by_reason], by=list(Date=dta$date), FUN=sum)

# start, end data
start_date <- as.Date("2020-02-10")
end_date <- as.Date("2020-04-26")

# filter data frame on dates
df_sum <- df_sum %>% filter(Date >= start_date & Date <= end_date)
n_rows <- nrow(df_sum)
n_cols <- ncol(df_sum)

# weekend days
sats <- which(weekdays(df_sum$Date) == "Saturday")
suns <- which(weekdays(df_sum$Date) == "Sunday")

# melt data
df_sum_long <- melt(df_sum,id="Date")
df_sum_long$size <- c(rep(0.8,(n_rows*(n_cols-1))))

# colors
colors2 <- c("#d34f53","#008694","#0e5b90")

# plot 
diff_plot <-  ggplot(df_sum_long, aes( x = Date,
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
  
  ylab("Trip count (x 1,000,000)") +
  
  # x-axis range
  scale_x_date(breaks = "week",
               labels = date_format("%d\n%b"),
               limits = as.Date(c("2020-02-10","2020-04-27")),
               expand = c(0,0)) +
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(0,35250000),
                     breaks = seq(0,36000000,6000000),
                     labels = c("0","6","12","18","24","30",""),
                     expand = c(0, 0)) +
  
  coord_cartesian(clip = "off") +
  
  # circle plot (by type)
  annotate("point",
           size = 0.1,
           shape = 19,
           color = colors2,
           x = start_date,
           y = c(df_sum[1,2],df_sum[1,3],df_sum[1,4])) +
  
  # variable text (by reason)
  annotate("text",
           fontface = "bold",
           x = start_date,
           y = c(df_sum[1,2]-1500000, df_sum[1,3]+2000000, df_sum[1,4]-1500000),
           label = c("Total","Commuters","Non-commuters"),
           color = colors2,
           hjust = 0,
           size = 2.5) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-02-25"),
               xend = as.Date("2020-02-25"),
               y = 0,
               yend = 31000000,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-02-28"),
               xend = as.Date("2020-02-28"),
               y = 0,
               yend = 32500000,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-13"),
               xend = as.Date("2020-03-13"),
               y = 0,
               yend = 31000000,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-16"),
               xend = as.Date("2020-03-16"),
               y = 0,
               yend = 34500000,
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
           y = c(31000000,33000000,31000000,35250000),
           label = c("First case",
                     "Ban>1000",
                     "Ban>100",
                     "Closed schools"),
           hjust = 1,
           size = 2.5) +
  
  geom_segment(x = as.Date("2020-03-17"),
               xend = as.Date("2020-03-17"),
               y = 0,
               yend = 34500000,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-03-20"),
               xend = as.Date("2020-03-20"),
               y = 0,
               yend = 32000000,
               size = 0.2,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-25"),
               xend = as.Date("2020-03-25"),
               y = 0,
               yend = 31000000,
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
           y = c(35250000,
                 32500000,
                 31000000),
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
geom_segment(x = as.Date("2020-04-26"),
             xend = as.Date("2020-04-26"),
             y = 28000000,
             yend = 31000000,
             size = 0.2,
             color = 'grey',
             linetype = 'solid') +
  
  # annotate weekend and Eastern description
  annotate("text",
           fontface = c("plain","bold"),
           color = c('grey25','grey'),
           x = c(as.Date("2020-04-26")-0.5,as.Date("2020-04-13")),
           y = c(31000000,29000000),
           label = c("Weekend","Eastern"),
           hjust = 1,
           size = c(2.5,2)) +
  
  # bullet on dashed vertical lines
  annotate("point",
           size = 0.3,
           shape = 19,
           color = 'grey',
           x = as.Date(c("2020-02-25","2020-02-28","2020-03-13","2020-03-16","2020-03-17","2020-03-20","2020-03-25","2020-04-26")),
           y = c(31000000,32500000,31000000,34500000,34500000,32000000,31000000,31000000))

# horizontal grid lines
diff_plot <- diff_plot + lapply(c(6000000,12000000,18000000,24000000,30000000), function(i) { geom_segment(aes(x=start_date, 
                                                                                                               xend=end_date+.5, 
                                                                                                               y=i, 
                                                                                                               yend=i), 
                                                                                                           size=0.1, 
                                                                                                           color="grey")})

# shaded weekend areas
diff_plot <- diff_plot + lapply(seq(1,11,1), function(i) {geom_rect(data=df_sum_long[1,],
                                                                    aes(xmin=df_sum[sats[i],1]+.5, xmax=df_sum[suns[i],1]+.5,
                                                                        ymin=0, ymax=30000000),
                                                                    color=NA, fill='grey', alpha=.3)})

# shade Eastern Monday
diff_plot <- diff_plot + geom_rect(data=df_sum_long[1,],
                                   aes(xmin=as.Date("2020-04-12")+.5, xmax=as.Date("2020-04-13")+.5,
                                       ymin=0, ymax=30000000),
                                   color=NA, fill='grey', alpha=.3)

# plot lines on top of everything
diff_plot <- diff_plot + geom_line(aes(group=variable), size = df_sum_long$size)

# final plot
diff_plot

# save plot
ggsave(filename = "./Plots/Fig_1/diff_mobility_by_reason.pdf", plot = diff_plot, width = 6, height = 3, bg = "transparent")