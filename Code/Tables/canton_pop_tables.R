library("here")
library("xtable")

rm(list=ls())

#### calculate population per month of 2020 #####

dta_m <- read_csv(here::here("Data","BFS_canton_population","canton_pop_2020_month.csv"))

print.xtable(
  print(xtable(dta_m, digits = 0), format.args = list(big.mark = " ")), 
  include.colnames =  T,
  include.rownames = F,
  hline.after = NULL,
  only.contents = T,
  sanitize.text.function = identity)



#### Quarterly population 2020 #####

dta_q <- read_csv(here::here("Data","BFS_canton_population","canton_pop_2020_quarter.csv"))

print.xtable(
  print(xtable(dta_q, digits = 0), format.args = list(big.mark = " ")), 
  include.colnames =  T,
  include.rownames = F,
  hline.after = NULL,
  only.contents = T,
  sanitize.text.function = identity)












