library(here)
library(lattice)
library(stargazer)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

rm(list=ls())

outpath <- here::here("Plots/mobility_validation/")

###############################################################################
### diff plot foreign cross border commuters                                ###
###############################################################################

# import data
dta <- read.csv(here::here("Data","BFS_cross_border_commuters", "px-x-0302010000_105_20210329-091959.csv"), sep=";")
canton_codes <- read_csv(here::here("Data","Canton_codes.csv"))

dta <- dta %>% filter(Arbeitskanton != "Schweiz")
canton_codes <- canton_codes %>% filter(canton_names != "Liechtenstein")

# rename cantons
dta <- arrange(dta, Arbeitskanton)
canton_codes <- arrange(canton_codes, MIP_canton_names)

dta$Arbeitskanton <- canton_codes$MIP_canton_names
dta$Arbeitskanton[23] <- "Waadt2"
dta$Arbeitskanton[24] <- "Waadt"
dta$Arbeitskanton[23] <- "Wallis"

# join canton codes
colnames(dta)[1] <- "MIP_canton_names"
dta <- merge(dta, canton_codes, by = "MIP_canton_names")
dta <- dta %>% select(canton_codes, X2019Q1, X2019Q2, X2019Q3, X2019Q4, X2020Q1, X2020Q2, X2020Q3, X2020Q4)

# re-format with year factor
dta$year <- 0
dta2019 <- dta %>% select(canton_codes, year, X2019Q1, X2019Q2, X2019Q3, X2019Q4)
dta2020 <- dta %>% select(canton_codes, year, X2020Q1, X2020Q2, X2020Q3, X2020Q4)

dta2019$year <- 2019
dta2020$year <- 2020

colnames(dta2019) <- colnames(dta2020) <- c("canton_codes","year","Q1","Q2","Q3","Q4")

# merge rows
dta <- rbind(dta2019,dta2020)

# wide to long format
dta <- melt(dta, id.vars=c("canton_codes", "year"))
colnames(dta) <- c("canton_codes","year","quarter","commuters")

# pre-processing
dta$canton_codes <- as.factor(dta$canton_codes)
dta$year <- as.factor(dta$year)
dta$quarter <- as.factor(dta$quarter)
dta$commuters <- dta$commuters/1000

# reorder quarters
dta$year <- relevel(dta$year, "2019")

# plot
plot_values <- barchart(commuters ~ quarter | canton_codes, 
                        groups = year,
                        data=dta, as.table=TRUE,
                        xlab="Quarter", ylab="Number of foreign cross-border commuters (x 1,000)",
                        par.settings = list(strip.background=list(col="lightgrey")),
                        auto.key = list(space = "inside", corner = c(1,0), title = "Year", cex.title = 1))

# write pdf
pdf(file = paste0(outpath, "cross_border_commuters_2019_2020.pdf"), width = 8, height = 5)
  plot_values
dev.off()
  
