
library(magrittr)
library(tidyverse)

rm(list=ls())

l <- locale(encoding = "latin1")

## ----------------------------------------------------------------
##                    Preprocessing Function                     -
## ----------------------------------------------------------------

prep <- function(df) {
  names(df) <- str_replace_all(names(df), c("\\+" = "plus", "-" = "."))
  return(
    df %>%
      rename_all(~ str_replace_all(., c(
        "Datum" = "date",
        "Beschreibung" = "measure",
        "Typ" = "type",
        "Auspr?gung" = "category",
        "Kanton" = "canton",
        "Alter" = "age",
        "Haushaltsgr?sse" = "household",
        "Personen" = "person",
        "Person" = "person",
        "Ja" = "yes",
        "Nein" = "no",
        "M?nnlich" = "male",
        "Weiblich" = "female",
        "St?dtisch" = "city",
        "L?ndlich" = "rural",
        "Nicht_Erwerbst?tig" = "unemployed",
        "Erwerbst?tig" = "employed",
        "In_Ausbildung" = "education",
        "Abotyp" = "subscription",
        "Halbtax/Strecken./Verbundabo" = "halbtax",
        "kein_Abo" = "none",
        "Auto" = "car",
        "Kinder" = "children"
      ))) %>%
      rename_all(tolower) %>%
      mutate_if(~ class(.) == "character", ~ str_replace_all(., c(
        "Median" = "median",
        "Mittelwert" = "mean",
        "Distanz" = "distance",
        "Radius" = "radius",
        "weniger als 500 Meter" = "d_500m.less",
        "weniger als 2 Km" = "d_2km.less",
        "500 Meter - 2 Kilometer" = "d_500m.2km",
        "2 - 10 Kilometer" = "d_2km.10km",
        "10 - 20 Kilometer" = "d_10km.20km",
        "20 - 50 Kilometer" = "d_20km.50km",
        "50 - 100 Kilometer" = "d_50km.100km",
        "100\\+\\+ Kilometer" = "d_100km.more"
      ))) %>%
      mutate(weekday = factor(weekdays(date, abbreviate = T),
        ordered = T,
        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      ), .after = date)
  )
}

## ---------------------------------------------------------------
##                          Distances                           -
## ---------------------------------------------------------------

# total distances and radii travelled per day
distances_radii <- read_csv(here::here("Data","Intervista_AG","Mittelwerte_und_Median_pro_Tag.csv"), locale = l)

# distances in km by mode
modes <- read_csv(here::here("Data","Intervista_AG","Modalsplit_pro_Tag.csv"), locale = l)

# distances in km by purpose
purposes <- read_csv(here::here("Data","Intervista_AG","Mobilitaetszweck_pro_Tag.csv"), locale = l)

distances_radii %<>% prep()
modes %<>% prep()
purposes %<>% prep()

distances_radii %<>%
  left_join(purposes %>% select(-total) %>%
    mutate(measure = "distance", type = "mean"), by = c("date", "weekday", "measure", "type"))

distances_radii %<>%
  left_join(modes %>% select(-total) %>%
    mutate(measure = "distance", type = "mean"), by = c("date", "weekday", "measure", "type"), suffix = c("_purpose", "_mode"))

distances_radii %<>%
  rename_at(vars(total:other_mode), list(~ paste0("mobility_", .)))

write_csv(distances_radii, here::here("Data","Intervista_AG","distances_radii.csv"))


