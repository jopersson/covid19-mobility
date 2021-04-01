require(reshape2)
require(ggplot2)
require(scales)
require(readr)
require(grid)
require(magrittr)

rm(list=ls())

###############################################################################
### diff plot train mobility by canton                                      ###
###############################################################################

# read data
dta <- read_csv(file=here::here("Data","MIP_data","Train_trips_ColumnC","train_trips_canton_per_day_2020.csv"))


# mutate data
dta <- dta %>% mutate(X1 = as.Date(X1)) %>% rename(Date = X1)

# duplicate data frame for log levels
dta_ln <- dta

# calculate variables in logs
for(col in colnames(dta_ln)[2:27]){
  dta_ln[,col] <- log(dta[,col])
}

# start and end data
start_date <- as.Date("2020-02-10")
end_date <- as.Date("2020-04-26")

# filter data frame on dates
dta_ln <- dta_ln %>% filter(Date >= start_date & Date <= end_date)

# number of rows and columns
n_rows <- nrow(dta_ln)
n_cols <- ncol(dta_ln)

# weekend days
sats <- which(weekdays(dta_ln$Date) == "Saturday")
suns <- which(weekdays(dta_ln$Date) == "Sunday")

# melt data
melt_df <- melt(dta_ln, id="Date")

# add line width and color columns to data frame
melt_df$size <- c(rep(0.1,(n_cols-1)*n_rows))
melt_df$color <- c(rep('grey',(n_cols-1)*n_rows))

# custom line width and color
melt_df$size[melt_df$variable=='Zurich'] <- 0.6
melt_df$color[melt_df$variable=='Zurich'] <- "#e5921a"

melt_df$size[melt_df$variable=='Geneve'] <- 0.6
melt_df$color[melt_df$variable=='Geneve'] <- "#12868e"

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
  ylab("Log train trips") + 
  
  # axes ticks and horizontal lines
  scale_y_continuous(limits = c(6,16),
                     breaks = seq(7,15,1),
                     labels = c("7","8","9","10","11","12","13","14","15"),
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
           y = c(11,10.6,12.6),
           label = c("Geneva","Ticino","Zurich"),
           color = c("#12868e","#884c6e",'#e5921a'),
           hjust = 0,
           size = c(2.5,2.5,2.5)) +
  
  # draw vertical lines
  geom_segment(x = as.Date("2020-02-25"),
               xend = as.Date("2020-02-25"),
               y = 6,
               yend = 15.25,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-13"),
               xend = as.Date("2020-03-13"),
               y = 6,
               yend = 15.25,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-16"),
               xend = as.Date("2020-03-16"),
               y = 6,
               yend = 16,
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
           y = c(15.25,
                 15.25,
                 16),
           label = c("First case",
                     "Ban>100",
                     "Closed schools"),
           hjust = 1,
           size = 2.5) +

  # draw vertical lines
  geom_segment(x = as.Date("2020-03-18"),
               xend = as.Date("2020-03-18"),
               y = 6,
               yend = 16,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  geom_segment(x = as.Date("2020-03-20"),
               xend = as.Date("2020-03-20"),
               y = 6,
               yend = 15.6,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +

  geom_segment(x = as.Date("2020-03-25"),
               xend = as.Date("2020-03-25"),
               y = 6,
               yend = 15.25,
               size = 0.1,
               color = 'grey',
               linetype = 'dashed') +
  
  # geom_segment(x = as.Date("2020-03-24"),
  #              xend = as.Date("2020-03-24"),
  #              y = 6,
  #              yend = 15.25,
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
           y = c(16,
                 15.6,
                 15.25),
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
           y = c(14.75,
                 15.25),
           label = c("Easter",
                     "Weekend"),
           hjust = 1,
           size = c(2,
                    2.5)) +
  
  # draw vertical line (weekend)
  geom_segment(x = as.Date("2020-04-26"),
               xend = as.Date("2020-04-26"),
               y = 15.25,
               yend = 13.75,
               size = 0.2,
               color = 'grey',
               linetype = 'solid') +

  # circle on dashed vertical lines
  annotate("point",
           size = 0.3,
           shape = 19,
           color = 'grey',
           x = as.Date(c("2020-02-25","2020-03-13","2020-03-16","2020-03-18","2020-03-20","2020-03-25","2020-04-26")),
           y = c(15.25, 15.25, 16, 16, 15.6, 15.25, 15.25))


# horizontal grid lines
diff_plot <- diff_plot + lapply(seq(7,15,1), function(i) { geom_segment(aes(x=start_date, 
                                                                            xend=end_date+.5, 
                                                                            y=i, 
                                                                            yend=i), 
                                                                        size=0.1, 
                                                                        color="grey")})
# shaded weekend areas
diff_plot <- diff_plot + lapply(seq(1,11,1), function(i) {geom_rect(data=melt_df[1,],
                                                                    aes(xmin=melt_df[sats[i],1]+.5, xmax=melt_df[suns[i],1]+.5,
                                                                        ymin=6, ymax=15),
                                                                    color=NA, fill='grey', alpha=.3)})

# shade Eastern Monday 2020
diff_plot <- diff_plot + geom_rect(data=melt_df[1,],
                                   aes(xmin=as.Date("2020-04-12")+.5, xmax=as.Date("2020-04-13")+.5,
                                       ymin=6, ymax=15),
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
ggsave(filename = "./Plots/parallel_trends/diff_mobility_by_canton_train.pdf", plot = diff_plot, width = 8, height = 3)