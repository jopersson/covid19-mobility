library("purrr")
library("dplyr")
library("readr")
library("here")

rm(list=ls())

###############################################################################
### load 2019 canton populations                                            ###
###############################################################################

# load data
dta <- read.csv(file=here::here("Data","BFS_canton_population","px-x-0103010000_119_20210329-091343.csv"), sep=";")
canton_codes <- read.csv(file=here::here("Data","canton_codes.csv"), sep=",")


# sum permanent and non-permanenent resident populations within cantons
colnames(dta) = c("year","canton","pop_type","canton_pop")
dta <- aggregate(dta$canton_pop, by=list(canton=dta$canton), FUN=sum)

# join correct canton names and correct them
dta <- cbind(dta, canton_codes$MIP_canton_names[1:26])
colnames(dta) <- c("canton","canton_pop","MIP_canton_names")
dta$MIP_canton_names[24] <- "Waadt"
dta$MIP_canton_names[23] <- "Wallis"

dta$MIP_canton_names[21] <- "Ticino"
dta$MIP_canton_names[20] <- "Thurgau"

# join canton codes
dta <- left_join(dta, canton_codes[1:26,], by = "MIP_canton_names")

# keep only canton codes and population numbers
dta <- dta %>% select(canton_codes, canton_pop)

# sort on canton codes alphabetically
dta <- dta[order(dta$canton_codes),]

# write file
write_csv(dta, path=here::here("Data","BFS_canton_population","canton_pop_2019.csv"))



###############################################################################
### load 2020 canton populations per quarter                                ###
###############################################################################

pop_2019 <- read.csv(here::here("Data","BFS_canton_population","canton_pop_2019.csv"), sep=",")
canton_codes <- read.csv(here::here("Data","canton_codes.csv"), sep=",")

pop_q1 <- read_excel( here::here("Data","BFS_canton_population","cc-d-01.01.04.xlsx"), sheet = "1_Quartal", skip = 3)
pop_q2 <- read_excel( here::here("Data","BFS_canton_population","cc-d-01.01.04.xlsx"), sheet = "2_Quartal", skip = 3)
pop_q3 <- read_excel( here::here("Data","BFS_canton_population","cc-d-01.01.04.xlsx"), sheet = "3_Quartal", skip = 3)

# sum permanent and non-permanent populations
pop_q1$q1 <- pop_q1$'Total...2' + pop_q1$'Total...11'
pop_q2$q2 <- pop_q2$'Total...2' + pop_q2$'Total...11'
pop_q3$q3 <- pop_q3$'Total...2' + pop_q3$'Total...11'

# only keep total population
pop_q1 <- pop_q1 %>%
  select(...1, q1) 
pop_q2 <- pop_q2 %>%
  select(...1, q2) 
pop_q3 <- pop_q3 %>%
  select(...1, q3)

# join
dta_q <- list( pop_q1, pop_q2, pop_q3) %>% reduce(left_join, by = "...1")
colnames(dta_q)[1] <- "canton"

# filter out all rows except canton
dta_q <- dta_q  %>%
  filter(canton != "Schweiz" & canton != "Liechtenstein" & canton != "Genferseeregion" & canton != "Espace Mittelland" &
           canton != "Nordwestschweiz" & canton != "Ostschweiz" & canton != "Zentralschweiz") 

# remove non-data rows and order on canton names
dta_q <- dta_q[1:26,]
dta_q <- dta_q[order(dta_q$canton),]

# remove Liechtenstein from data to join with
canton_codes<- canton_codes %>%
  filter(canton_names  != "Liechtenstein") 

# merge
dta_q <- cbind(canton_codes$MIP_canton_names, dta_q) %>% select(-canton)
colnames(dta_q)[1] <- "MIP_canton_names"

# merge canton_codes
dta_q <- left_join(canton_codes, dta_q, by = "MIP_canton_names") %>% select(canton_codes, q1, q2, q3)

# merge with 2019 populations
dta_q <- left_join(pop_2019, dta_q, by = "canton_codes") %>% select(canton_codes, canton_pop, q1, q2, q3)

# rename columns for table
colnames(dta_q) <- c("Canton code", "2019", "Q1 2020", "Q2 2020", "Q3 2020")

## saving
write_csv(dta_q, path=here::here("Data","BFS_canton_population","canton_pop_2020_quarter.csv"))



###############################################################################
### load calculated 2020 canton populations per month                       ###
###############################################################################

rm(list=ls())

# read data
pop_2019 <- read.csv(here::here("Data","BFS_canton_population","canton_pop_2019.csv"), sep=",")
canton_codes <- read.csv(here::here("Data","canton_codes.csv"), sep=",")

deaths_jan <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.02.01.03.xlsx"), sheet = "Januar 2020p", skip = 4)
deaths_feb <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.02.01.03.xlsx"), sheet = "Februar 2020p", skip = 4)
deaths_mar <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.02.01.03.xlsx"), sheet = "März 2020p", skip = 4)
deaths_apr <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.02.01.03.xlsx"), sheet = "April 2020p", skip = 4)

births_jan <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.01.01.02.xlsx"), sheet = "Januar 2020p", skip = 5)
births_feb <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.01.01.02.xlsx"), sheet = "Februar 2020p", skip = 5)
births_mar <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.01.01.02.xlsx"), sheet = "März 2020p", skip = 5)
births_apr <- read_excel(here::here("Data","BFS_canton_population","cc-d-01.04.01.01.02.xlsx"), sheet = "April 2020p", skip = 5)

deaths_jan <- deaths_jan[2:27,1:2]
deaths_feb <- deaths_feb[2:27,1:2]
deaths_mar <- deaths_mar[2:27,1:2]
deaths_apr <- deaths_apr[2:27,1:2]
births_jan <- births_jan[2:27,1:2]
births_feb <- births_feb[2:27,1:2]
births_mar <- births_mar[2:27,1:2]
births_apr <- births_apr[2:27,1:2]

# rename missing column name to join on
colnames(births_jan)[1] <- colnames(births_feb)[1] <- colnames(births_mar)[1] <- colnames(births_apr)[1] <- "Kantone"

# join
dta_m <- list( deaths_jan, deaths_feb, deaths_mar, deaths_apr,
               births_jan, births_feb, births_mar, births_apr) %>% reduce(left_join, by = "Kantone")

colnames(dta_m) <- c("canton","d_jan","d_feb","d_mar","d_apr","b_jan","b_feb","b_mar","b_apr")

# remove Liechtenstein from data to join with
canton_codes<- canton_codes %>%
  filter(canton_names  != "Liechtenstein") 

# rename for merging
dta_m$canton[22] <- "Waadt"
dta_m$canton[23] <- "Wallis"
dta_m$canton[17] <- "Sankt-Gallen"

# order for merging
dta_m <- dta_m[order(dta_m$canton),]
canton_codes <- canton_codes[order(canton_codes$MIP_canton_names),]

# merge
dta_m <- cbind(canton_codes$MIP_canton_names, dta_m) %>% select(-canton)
colnames(dta_m)[1] <- "MIP_canton_names"

# merge canton_codes
dta_m <- left_join(canton_codes, dta_m, 
                   by = "MIP_canton_names") %>% select(canton_codes, 
                                                       d_jan, d_feb, d_mar, d_apr, b_jan, b_feb, b_mar, b_apr)

# merge with 2019 populations
dta_m <- left_join(pop_2019, dta_m, by = "canton_codes")

# recursively calculate monthly populations
dta_m$jan_2020 <- dta_m$canton_pop + dta_m$b_jan - dta_m$d_jan
dta_m$feb_2020 <- dta_m$jan_2020   + dta_m$b_feb - dta_m$d_feb
dta_m$mar_2020 <- dta_m$feb_2020   + dta_m$b_mar - dta_m$d_mar
dta_m$apr_2020 <- dta_m$mar_2020   + dta_m$b_apr - dta_m$d_apr

# only keep population numbers
dta_m <- dta_m %>% select(canton_codes, canton_pop, jan_2020, feb_2020, mar_2020, apr_2020)

# rename columns for table
colnames(dta_m) <- c("Canton code", "2019", "Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020")

## saving
write_csv(dta_m, path=here::here("Data","BFS_canton_population","canton_pop_2020_month.csv"))

