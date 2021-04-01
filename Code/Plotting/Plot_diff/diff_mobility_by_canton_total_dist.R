require(reshape2)
require(ggplot2)
require(scales)
require(readr)
require(grid)
require(magrittr)

rm(list=ls())

###############################################################################
### diff plot average distance travelled by canton                          ###
###############################################################################

# read data
dta <- read_csv(file=here::here("Data","MIP_data","Avg_travel_distances","Avg_daily_travel_distance_cantons.csv"))

# mutate data
dta <- dta %>% mutate(date = as.Date(date)) %>% rename(Date = date)

# long to wide format
dta <- spread(dta, canton_codes, avg_distance_km)

# start and end data
start_date <- as.Date("2020-02-10")
end_date <- as.Date("2020-04-26")

# filter data frame on dates
dta <- dta %>% filter(Date >= start_date & Date <= end_date)

# number of rows and columns
n_rows <- nrow(dta)
n_cols <- ncol(dta)

# weekend days
sats <- which(weekdays(dta$Date) == "Saturday")
suns <- which(weekdays(dta$Date) == "Sunday")

# melt data
melt_df <- melt(dta, id="Date")

# add line width and color columns to data frame
melt_df$size <- c(rep(0.1,(n_cols-1)*n_rows))
melt_df$color <- c(rep('grey',(n_cols-1)*n_rows))

# custom line width and color
melt_df$size[melt_df$variable=='ZH'] <- 0.6
melt_df$color[melt_df$variable=='ZH'] <- "#e5921a"

melt_df$size[melt_df$variable=='GE'] <- 0.6
melt_df$color[melt_df$variable=='GE'] <- "#12877e"

melt_df$size[melt_df$variable=='TI'] <- 0.6
melt_df$color[melt_df$variable=='TI'] <- "#884c6e"


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
  ylab("Average daily distance travelled (km)") + 
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(0,80),
                     breaks = seq(0,70,10),
                     labels = c("0","10","20","30","40","50","60","70"),
                     expand = c(0, 0)) +
  
  scale_x_date(breaks = "week",
               labels = date_format("%d\n%b"),
               limits = as.Date(c("2020-02-10","2020-04-27")),
               expand = c(0,0)) +
  
  coord_cartesian(clip = "off") +
  
  # annotate canton names (left-align)
  annotate("text",
           fontface = c("bold","bold","bold"),
           x = as.Date(c("2020-02-17","2020-03-02","2020-04-15")),
           y = c(23,31,22),
           label = c("Geneva","Ticino","Zurich"),
           color = c("#12877e","#884c6e",'#e5921a'),
           hjust = 0,
           size = c(2.5,2.5,2.5)) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-02-25"),
               xend = as.Date("2020-02-25"),
               y = 0,
               yend = 72,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-13"),
               xend = as.Date("2020-03-13"),
               y = 0,
               yend = 72,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-16"),
               xend = as.Date("2020-03-16"),
               y = 0,
               yend = 80,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  # annotate event description (right-aligned)
  annotate("text",
           fontface = "plain",
           color = 'grey25',
           x = c(as.Date("2020-02-25")-0.75,
                 as.Date("2020-03-13")-0.75,
                 as.Date("2020-03-16")-0.5),
           y = c(72,
                 72,
                 80),
           label = c("First case",
                     "Ban>100",
                     "Closed schools"),
           hjust = 1,
           size = 2.5) +

  # draw vertical lines
  geom_segment(x = as.Date("2020-03-18"),
               xend = as.Date("2020-03-18"),
               y = 0,
               yend = 80,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-20"),
               xend = as.Date("2020-03-20"),
               y = 0,
               yend = 75,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-25"),
               xend = as.Date("2020-03-25"),
               y = 0,
               yend = 72,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  # geom_segment(x = as.Date("2020-03-24"),
  #              xend = as.Date("2020-03-24"),
  #              y = 6,
  #              yend = 72,
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
           y = c(80,
                 75,
                 72),
           label = c("Closed venues",
                     "Ban>5",
                     "Closed borders"),
           hjust = 0,
           size = 2.5) +

  # annotate weekend description
  annotate("text",
           fontface = c("bold",
                        "plain"),
           color = c('grey',
                     'grey25'),
           x = c(as.Date("2020-04-12"),
                 as.Date("2020-04-26")-0.5),
           y = c(67,
                 72),
           label = c("Easter",
                     "Weekend"),
           hjust = 1,
           size = c(2,
                    2.5)) +
  
  # draw vertical line (weekend)
  geom_segment(x = as.Date("2020-04-26"),
               xend = as.Date("2020-04-26"),
               y = 67.5,
               yend = 72.5,
               size = 0.2,
               color = 'grey',
               linetype = 'solid') +

  # circle on dashed vertical lines
  annotate("point",
           size = 0.3,
           shape = 19,
           color = 'grey',
           x = as.Date(c("2020-02-25","2020-03-13","2020-03-16","2020-03-18","2020-03-20","2020-03-25","2020-04-26")),
           y = c(72, 72, 80, 80, 75, 72, 72))


# horizontal grid lines
diff_plot <- diff_plot + lapply(seq(0,70,10), function(i) { geom_segment(aes(x=start_date, 
                                                                            xend=end_date+.5, 
                                                                            y=i, 
                                                                            yend=i), 
                                                                        size=0.1, 
                                                                        color="grey")})
# shaded weekend areas
diff_plot <- diff_plot + lapply(seq(1,11,1), function(i) {geom_rect(data=melt_df[1,],
                                                                    aes(xmin=melt_df[sats[i],1]+.5, xmax=melt_df[suns[i],1]+.5,
                                                                        ymin=0, ymax=70),
                                                                    color=NA, fill='grey', alpha=.3)})

# shade Eastern Monday 2020
diff_plot <- diff_plot + geom_rect(data=melt_df[1,],
                                   aes(xmin=as.Date("2020-04-12")+.5, xmax=as.Date("2020-04-13")+.5,
                                       ymin=0, ymax=70),
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
ggsave(filename = "./Plots/mobility_validation/diff_mobility_by_canton_total_dist.pdf", plot = diff_plot, width = 8, height = 3)