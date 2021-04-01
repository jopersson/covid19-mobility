library(dplyr)
library(magrittr)
library(tidyverse)
library(here)

# Round function
round_k <- function(x, k=2){
  trimws(format(round(x,k), nsmall=k, big.mark=","))
}

# Load data
df <- read_csv(file=here::here("Data","Policies","Covid_policies_consolidated_full_v2.csv"))

# to date format and rename column
df$X1 %<>% as.Date
colnames(df)[1] <- "Date"

### Tbl: Time Diff
npi_difs = function(type = "mean", 
                    npi_list = c("ban_1000","ban_100","closed_schools",
                                 "closed_stores-bars","ban_5","closed_borders"),
                    canton_list = sort(unique(df$canton))){
  
  # initialize table
  table_appendix =  data.frame(matrix(ncol = length(npi_list), nrow = length(npi_list)))
  colnames(table_appendix) = npi_list
  rownames(table_appendix) = npi_list
  
  # fill table with time deltas between NPI a and NPI b for given canton
  for (npi_a in npi_list) {
    for (npi_b in npi_list) {
      cantons_delta = c(rep(NA, length(canton_list)))
      cnt = 1
      
      for (canton_i in canton_list) {
        # skip same NPIs
        if (!(npi_a == npi_b)){
          # get date diff between npi_a and npi_b in canton_i
          dat_cnt <- df[df[["canton"]] == canton_i,]
          dates_npi_a <- dat_cnt[dat_cnt[, npi_a]==1,]$Date
          dates_npi_b <- dat_cnt[dat_cnt[, npi_b]==1,]$Date
          time_npi_a <- ifelse(length(dates_npi_a) == 0, NA, min(dates_npi_a))
          time_npi_b <- ifelse(length(dates_npi_b) == 0, NA, min(dates_npi_b))
          if (is.na(time_npi_a) | is.na(time_npi_b)) {
            delta <- NA
          } else {
            delta <- abs(as.double(time_npi_a-time_npi_b))
          }
          # add data to matrix
          cantons_delta[cnt] = delta
          cnt = cnt + 1
        }
      }
      
      if (type == "mean") {
        table_appendix[npi_a, npi_b] <- round_k(mean(cantons_delta, na.rm = TRUE),1)
      } 
      else if (type == "median") {
        table_appendix[npi_a, npi_b] <- round_k(median(cantons_delta, na.rm = TRUE),1)
      }
      else {
        print(cantons_delta)
        print(round_k(sd(cantons_delta, na.rm = TRUE),1))
        table_appendix[npi_a, npi_b] <- round_k(sd(cantons_delta, na.rm = TRUE),1)
      }
      cnt = 1
    }
  }
  return(table_appendix)
}

table_means = npi_difs(type = "mean")
table_means
table_medians = npi_difs(type = "median")
table_medians
table_sd = npi_difs(type = "sd")
table_sd

# to LaTex
table_means[(table_means) == "NaN"] = "{---}"
table_medians[(table_medians)  == "NA"] = "{---}"
table_sd[(table_sd)  == "NA"] = "{---}"

xtable::print.xtable(
  file = here::here("Data","Policies","NPI_diff_table","npi_diff_mean.tex"),
  xtable::xtable(table_means), 
  include.colnames =  F,
  include.rownames = T,
  hline.after = NULL,
  only.contents = T,
  sanitize.text.function = identity)

xtable::print.xtable(
  file = here::here("Data","Policies","NPI_diff_table","npi_diff_median.tex"),
  xtable::xtable(table_medians), 
  include.colnames = F,
  include.rownames = T,
  hline.after = NULL,
  only.contents = T,
  sanitize.text.function = identity) 

xtable::print.xtable(
  file = here::here("Data","Policies","NPI_diff_table","npi_diff_sd.tex"),
  xtable::xtable(table_sd), 
  include.colnames = F,
  include.rownames = T,
  hline.after = NULL,
  only.contents = T,
  sanitize.text.function = identity) 
