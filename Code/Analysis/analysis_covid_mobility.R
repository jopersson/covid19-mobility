library("rstan")
library("brms")
library("loo")
library("bayesplot")
library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("tidybayes")
library("lubridate")
library("here")
library("stringr")
library("readxl")
library("texreg")
library("broom")
library("dotwhisker")
library("tibble")
library("cowplot")
library("haven")
library("gridExtra")


rm(list=ls())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(12345)

source(here::here("Code","helper_functions.R"))

outpath <- here::here("Plots/covid_analysis/")

dta <- read_csv(file=here::here("Data","Merged_panel_data.csv"))


#--------------------- Data preparation -------------------------

# variable pre-processing
dta <- dta %>%
  mutate(canton_codes = as.factor(canton_codes)) %>%
  mutate(positivity_rate = as.numeric(positivity_rate),
         tests = as.numeric(tests)) %>%
  mutate_at(vars(contains("trips")), as.numeric ) %>%
  mutate(t_num = as.numeric(t)) %>%
  mutate(t_num = if_else(t_num<0, 0, t_num)) %>%
  mutate(t_num_sq = t_num^2,
         t_num_ln = log(1+t_num),
         t_f = as.factor(t_num),
         wd = as.factor(wd),
         date_f = as.factor(date),
         week_f = as.factor(strftime(date, format = "%V")))

# Set monday as reference weekday
dta <- within(dta, wd <- relevel(wd, ref = "Monday"))

### create lags of policies 
dta <- dta %>% 
  group_by(canton_codes) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate_at(vars(contains("trips")),
            list(lag0 = ~ ., 
                 lag7=~dplyr::lag(.,n=7),
                 lag8=~dplyr::lag(.,n=8),
                 lag9=~dplyr::lag(.,n=9),
                 lag10=~dplyr::lag(.,n=10),
                 lag11=~dplyr::lag(.,n=11),
                 lag12=~dplyr::lag(.,n=12),
                 lag13=~dplyr::lag(.,n=13),
                 lag14=~dplyr::lag(.,n=14),
                 lag15=~dplyr::lag(.,n=15),
                 lag16=~dplyr::lag(.,n=16),
                 lag17=~dplyr::lag(.,n=17),
                 lag18=~dplyr::lag(.,n=18),
                 lag19=~dplyr::lag(.,n=19),
                 lag20=~dplyr::lag(.,n=20)
            )) %>% 
  ungroup()

# subset time frame
dta <- dta %>% 
  filter(date >= as.Date("2020-02-24")) %>%
  filter(date <= as.Date("2020-04-05")) %>% 
  filter(t_num>0) # only after 1st case

# remove some policies since we can't identify all of them
dta <- dta %>% dplyr::select(-contains("ban_30"),
                             -contains("museum"),
                             -contains("hygiene"),
                             -contains("hair"),
                             -contains("no_test"),
                             -contains("eth_ban_1000"))

# create canton-standardized number of test variable:
# daily number of tests in the country times share of population in canton
total_pop <- sum(unique(dta$canton_pop))
dta$tests_std <- dta$tests*(dta$canton_pop/total_pop)

# add log of number of tests and tests standardized to canton population size
dta <- dta %>%
  mutate(tests_ln = log(tests)) %>%
  mutate(tests_std_ln = log(tests_std))

# add percentage scaled version of test positivity rate
dta$positivity_rate_pc <- 100*dta$positivity_rate

# add within-group mean of time-varying predictors
dta_mean <- dta %>%
        group_by(canton_codes) %>%
        arrange(date,.by_group=TRUE) %>%
        dplyr::summarize(across( contains( c("trips", "t_num") ), mean, .names = "{col}_mean"))

dta <- full_join(dta, dta_mean, by = "canton_codes")
remove(dta_mean)

# load data
dta_dist <- read_csv(file=here::here("Data","MIP_data","Avg_travel_distances","Avg_daily_travel_distance_cantons.csv"))

# pre-processing
dta_dist$date <- as.Date(dta_dist$date)
dta_dist <- dta_dist %>% 
  filter(date >= as.Date("2020-02-24")) %>% 
  filter(date <= as.Date("2020-04-05"))
dta <- full_join(dta, dta_dist, by = c("date","canton_codes"))
remove(dta_dist)


#--------------------- Main Model -------------------------


range_lags <- 7:13

mod_bag_trend <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
    for ( s in range_lags ) {
    priors <- c(set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                  "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_falle ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_trend[[paste(m, s)]] <- brm(model_formula,
                                       family = negbinomial(link = "log", link_shape = "log"),
                                       data = dta %>% dplyr::select(bag_falle, 
                                                                    !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                    !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                    canton_pop, canton_codes, wd, t_num_ln, t_num_ln_mean),
                                       prior = priors, 
                                       inits = 0, 
                                       chains = 4,
                                       iter = 4000,
                                       thin = 2, 
                                       cores = parallel::detectCores(), 
                                       control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                       seed = 12345 )
  }
}


#--------------------- Robustness checks: Alternative time-effect specifications -------------------------


range_lags <- 7:13

nb_bag_week_trend <- list()
nb_bag_sqtrend <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
  for ( s in range_lags ) {
    
    priors1 <- c(set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                    set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                    set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                    set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                    set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                      "wdFriday", "wdSaturday", "wdSunday")))

    priors2 <- c(set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                 set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                 set_prior("normal(0, 1)", class = "b", coef = "t_num"),
                 set_prior("normal(0, 5)", class = "b", coef = "t_num_mean"),
                 set_prior("normal(0, 1)", class = "b", coef = "t_num_sq"),
                 set_prior("normal(0, 5)", class = "b", coef = "t_num_sq_mean"),
                 set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                   "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_falle ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_week_trend[[paste(m, s)]] <- brm(model_formula,
                                            family = negbinomial(link = "log", link_shape = "log"),
                                            data = dta %>% dplyr::select(bag_falle, 
                                                                         !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                         !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                         canton_pop, canton_codes, wd, week_f, t_num_ln, t_num_ln_mean),
                                            prior = priors1, 
                                            inits = 0, 
                                            chains = 4,
                                            iter = 4000,
                                            thin = 2, 
                                            cores = parallel::detectCores(), 
                                            control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                            seed = 12345 )
    
    nb_bag_sqtrend[[paste(m, s)]] <- brm(model_formula,
                                         family = negbinomial(link = "log", link_shape = "log"),
                                         data = dta %>% dplyr::select(bag_falle, 
                                                                      !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                      !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                      canton_pop, canton_codes, wd, t_num, t_num_mean, t_num_sq, t_num_sq_mean),
                                         prior = priors2, 
                                         inits = 0, 
                                         chains = 4,
                                         iter = 4000,
                                         thin = 2, 
                                         cores = parallel::detectCores(), 
                                         control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                         seed = 12345 )
    
  }
}



#--------------------- Robustness checks: Control for number of tests or positivity rate -------------------------


range_lags <- 7:13 

nb_bag_trend_tests <- list()
nb_bag_trend_positivity <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
  for ( s in range_lags ) {
    priors_tests <- c(set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                      set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                      set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                      set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                      set_prior("normal(1, 1)", class = "b", coef = "tests_std_ln"),
                      set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                           "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_falle ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_trend_tests[[paste(m, s)]] <- brm(model_formula,
                                             family = negbinomial(link = "log", link_shape = "log"),
                                             data = dta %>% dplyr::select(bag_falle, 
                                                                          !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                          !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                          canton_pop, canton_codes, wd, t_num_ln, t_num_ln_mean, tests_std_ln),
                                             prior = priors_tests, 
                                             inits = 0, 
                                             chains = 4,
                                             iter = 4000,
                                             thin = 2, 
                                             cores = parallel::detectCores(), 
                                             control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                             seed = 12345 )
    
    priors_positivity <- c(set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                            set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                            set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                            set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                            set_prior("normal(1, 1)", class = "b", coef = "positivity_rate_pc"),
                            set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                              "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_falle ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_trend_positivity[[paste(m, s)]] <- brm(model_formula,
                                                   family = negbinomial(link = "log", link_shape = "log"),
                                                   data = dta %>% dplyr::select(bag_falle, 
                                                                                !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                                !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                                canton_pop, canton_codes, wd, t_num_ln, t_num_ln_mean, positivity_rate_pc),
                                                   prior = priors_positivity, 
                                                   inits = 0, 
                                                   chains = 4,
                                                   iter = 4000,
                                                   thin = 2, 
                                                   cores = parallel::detectCores(), 
                                                   control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                                   seed = 12345 )
    }
}


#--------------------- Robustness check: Hospitalizations or deaths as dependent variable -------------------------


range_lags_long <- 10:20

nb_bag_trend_hosp <- list()
nb_bag_trend_death <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
  for ( s in range_lags_long ) {
    
    priors_hosp <- c( set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                      set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                      set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                      set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                      set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                        "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_hosp ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_trend_hosp[[paste(m, s)]] <- brm(model_formula,
                                            family = negbinomial(link = "log", link_shape = "log"),
                                            data = dta %>% dplyr::select(bag_hosp, 
                                                                         !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                         !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                         canton_pop, canton_codes, wd, t_num_ln, t_num_ln_mean),
                                            prior = priors_hosp, 
                                            inits = 0, 
                                            chains = 4,
                                            iter = 4000,
                                            thin = 2, 
                                            cores = parallel::detectCores(), 
                                            control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                            seed = 12345 )
    
    prior_death <- c( set_prior("normal(1, 1)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s) ),
                      set_prior("normal(0, 5)", class = "b", coef = paste0("ln_",m,"_trips_2020_lag",s,"_mean") ),
                      set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                      set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                      set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                        "wdFriday", "wdSaturday", "wdSunday")))
    
    model_formula <- brmsformula(bag_tode ~ . + offset(log(canton_pop)) + (1 | canton_codes) - canton_pop - canton_codes, center = TRUE)
    
    nb_bag_trend_death[[paste(m, s)]] <- brm(model_formula,
                                             family = negbinomial(link = "log", link_shape = "log"),
                                             data = dta %>% dplyr::select(bag_tode, 
                                                                          !!paste0("ln_",m,"_trips_2020_lag",s),
                                                                          !!paste0("ln_",m,"_trips_2020_lag",s,"_mean"),
                                                                          canton_pop, canton_codes, wd, t_num_ln, t_num_ln_mean),
                                             prior = prior_death, 
                                             inits = 0, 
                                             chains = 4,
                                             iter = 4000,
                                             thin = 2, 
                                             cores = parallel::detectCores(), 
                                             control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                             seed = 12345 )
  }
}


#--------------------- Plots for models with reported cases as dependent variable -------------------------

# See script "helper_functions.R" for the functions used below

mod_list <- c(nb_bag_trend, nb_bag_week_trend, nb_bag_sqtrend, nb_bag_trend_tests, nb_bag_trend_positivity)
plotname <- c("covid_nb_logtrend.pdf", "covid_nb_week_logtrend.pdf", "covid_nb_sqtrend.pdf",
              "covid_nb_logtrend_tests.pdf", "covid_nb_logtrend_positivity.pdf")

for(i in 1:length(plotname) ){
  
  # number of models per specification (no. of trip categories times no. of lags, less one as first is counted)
  mods <- length(trip_cat)*length(range_lags)-1
  
  # obtain model indices in lists
  mod_index <- (i+(i-1)*mods):((i+(i-1)*mods)+mods)
  
  # set model to obtain estimates from. 
  model_to_check <- mod_list[mod_index]
  
  # get model posterior estimates
  pred_total <- pred_est(model_to_check, "total", range_lags)
  pred_mode <- pred_est(model_to_check, "mode", range_lags)
  pred_purpose <- pred_est(model_to_check, "purpose", range_lags)
  
  # plot
  plot_total <- plot_pred(pred_total, "total")
  plot_mode <- plot_pred(pred_mode, "mode")
  plot_purpose <- plot_pred(pred_purpose, "purpose")
  
  # plot all side-by-side
  plot <- grid.arrange(plot_total, plot_mode, plot_purpose, widths = c(2.45, 2, 2))
  
  ggsave(filename = paste0(outpath, "/", plotname[i]), plot = plot, width = 12, height = 4)
}


#--------------------- Plots for models with reported deaths or hospitalizations as dependent variable -------------------------

# See script "helper_functions.R" for the functions used below

mod_list <- c(nb_bag_trend_hosp, nb_bag_trend_death)
plotname <- c("covid_nb_logtrend_hosp.pdf", "covid_nb_logtrend_death.pdf")

for(i in 1:length(plotname) ){
  
  # number of models per specification (no. of trip categories times no. of lags, less one as first is counted)
  mods <- length(trip_cat)*length(range_lags_long)-1
  
  # obtain model indices in lists
  mod_index <- (i+(i-1)*mods):((i+(i-1)*mods)+mods)
  
  # set model to obtain estimates from. 
  model_to_check <- mod_list[mod_index]
  
  # get model posterior estimates
  pred_total <- pred_est(model_to_check, "total", range_lags_long)
  pred_mode <- pred_est(model_to_check, "mode", range_lags_long)
  pred_purpose <- pred_est(model_to_check, "purpose", range_lags_long)
  
  # plot
  plot_total <- plot_pred(pred_total, "total")
  plot_mode <- plot_pred(pred_mode, "mode")
  plot_purpose <- plot_pred(pred_purpose, "purpose")
  
  # plot all side-by-side
  plot <- grid.arrange(plot_total, plot_mode, plot_purpose, widths = c(2.45, 2, 2))
  
  ggsave(filename = paste0(outpath, "/", plotname[i]), plot = plot, width = 12, height = 7)
}


#--------------------- Model diagnostics -------------------------


color_scheme_set("blue")
dep_vars <- c("total", "road", "train", "highway", "commuter", "non_commuter")

# set model to obtain estimates from
model_to_check <- nb_bag_trend

for( m in dep_vars){
  posterior_plots <- list()
  pp_plots <- list()
  param_plots <- list()
  
  for ( s in 1:7 ) {
    mod_obj <- model_to_check[[paste(m,s+6)]]
    
    # posterior chains plot
    posterior_plots[s] <- plot(mod_obj, 
                               pars = paste0("b_ln_",m,"_trips_2020_lag",s+6), 
                               fixed = TRUE) 
    posterior_plots[[s]]$bayesplots[[1]]$labels$x <- posterior_plots[[s]]$bayesplots[[2]]$labels$y <- c("")
    posterior_plots[[s]]$bayesplots[[1]] <- posterior_plots[[s]]$bayesplots[[1]] + ggtitle( paste("Cases ~ Lag",s+6,m,"trips", sep = " ") )
  
    # posterior predictive plot
    pp_plots[[s]] <- pp_check(mod_obj) + ggtitle( paste("Cases ~ Lag",s+6,m,"trips", sep = " ") )
  
    # estimates of all parameters except intercept
    param_plots[[s]] <- mcmc_plot(mod_obj, pars = c("wd","trips","t_num","sd","shape"))
  }
  
  # Trace & Density plot
  pdf(file = paste0(outpath, "cases_trace_density_",m,".pdf"), width = 12, height = 15)
  grid.arrange(posterior_plots[[1]]$bayesplots[[1]], posterior_plots[[1]]$bayesplots[[2]], 
               posterior_plots[[2]]$bayesplots[[1]], posterior_plots[[2]]$bayesplots[[2]],
               posterior_plots[[3]]$bayesplots[[1]], posterior_plots[[3]]$bayesplots[[2]],
               posterior_plots[[4]]$bayesplots[[1]], posterior_plots[[4]]$bayesplots[[2]],
               posterior_plots[[5]]$bayesplots[[1]], posterior_plots[[5]]$bayesplots[[2]],
               posterior_plots[[6]]$bayesplots[[1]], posterior_plots[[6]]$bayesplots[[2]],
               posterior_plots[[7]]$bayesplots[[1]], posterior_plots[[7]]$bayesplots[[2]],
               nrow = 7, ncol = 2)
  dev.off()
  
  # Posterior predictive plot
  pdf(file = paste0(outpath, "cases_pp_check_",m,".pdf"), width = 12, height = 12)
  grid.arrange(pp_plots[[1]], pp_plots[[2]], pp_plots[[3]], pp_plots[[4]], pp_plots[[5]], pp_plots[[6]], pp_plots[[7]],
               nrow = 7)

  
  acf_plots <- list()
  neff_plots <- list()
  rhat_plots <- list()
  
  for ( s in 1:7 ) {
    mod_obj <- model_to_check[[paste(m,s+6)]]
    
    # ACF for all model parameters
    acf_plot <- mcmc_acf( as.array(mod_obj, pars = paste0("b_ln_",m,"_trips_2020_lag",s+6), fixed = TRUE), lags = 10, size = 1 )
    acf_plot[[1]]$Parameter <- paste("Cases ~ Lag",s+6,"total trips", sep = " ")
    acf_plots[[s]] <- acf_plot
    
    # Neff for all model parameters
    neff_plots[[s]] <- mcmc_neff( neff_ratio(mod_obj), size = 3 ) + ggtitle( paste("Cases ~ Lag",s+6,m,"trips", sep = " ") )
    
    # Rhat for all model parameters
    rhat_plots[[s]] <- mcmc_rhat( rhat(mod_obj), size = 3  ) + ggtitle( paste("Cases ~ Lag",s+6,m,"trips", sep = " ") )
  }
  
  # ACF plot
  pdf(file = paste0(outpath, "cases_acf_",m,".pdf"), width = 12, height = 15)
  grid.arrange(acf_plots[[1]], acf_plots[[2]], acf_plots[[3]], acf_plots[[4]], acf_plots[[5]], acf_plots[[6]], acf_plots[[7]],
               ncol = 2)
  dev.off()
  
  # Neff plot
  pdf(file = paste0(outpath, "cases_neff_",m,".pdf"), width = 12, height = 15)
  grid.arrange(neff_plots[[1]], neff_plots[[2]], neff_plots[[3]], neff_plots[[4]], neff_plots[[5]], neff_plots[[6]], neff_plots[[7]],
               ncol = 2)
  dev.off()
  
  # Rhat plot
  pdf(file = paste0(outpath, "cases_rhat_",m,".pdf"), width = 12, height = 15)
  grid.arrange(rhat_plots[[1]], rhat_plots[[2]], rhat_plots[[3]], rhat_plots[[4]], rhat_plots[[5]], rhat_plots[[6]], rhat_plots[[7]],
               ncol = 2)

  # Plot of estimated Pareto tail shape parameter k against observation indices to check influential observations
  pdf(file = paste0(outpath, "cases_pareto_",m,".pdf"), width = 12, height = 15)
  par( mfrow = c(4,2), cex = 0.85 )
  for ( s in 1:7 ) {
    mod_obj <- model_to_check[[paste(m,s+6)]]
    x <- loo(mod_obj) 
    plot(x, diagnostic = "k", label_points = FALSE, moment_match = TRUE, main = paste("Cases ~ Lag",s+6,m,"trips", sep = " ") )
  }
  dev.off()
}
