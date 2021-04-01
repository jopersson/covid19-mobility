
#--------------------- Import data & packages -------------------------

library("rstan")
library("rstanarm")
library("brms")
library("bayesplot")
library("loo")
library("dplyr")
library("Hmisc")
library("ggplot2")
library("readr")
library("tidyr")
library("lubridate")
library("here")
library("readxl")
library("stringr")
library("texreg")
library("alpaca")
library("dotwhisker")
library("tibble")
library("gridExtra")


rm(list=ls())
set.seed(12345)

source(here::here("Code","helper_functions.R"))

outpath <- here::here("Plots/traffic_analysis/")

dta <- read_csv(file=here::here("Data","Merged_panel_data.csv"))


#--------------------- Data preparation -------------------------

# variable pre-processing
dta <- dta %>% 
  group_by(canton_codes) %>%
  arrange(date, .by_group = TRUE) %>%
  ungroup()

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

# remove some policies since we can't identify all of them
dta <- dta %>% dplyr::select(- contains("ban_30"),
                             - contains("museum"),
                             - contains("hygiene"),
                             - contains("hair"),
                             - contains("no_test"),
                             - contains("eth_ban_1000"))
dta <- subset( dta, select = -`eth_closed_stores-bars` )

# subset time frame
dta <- dta %>% 
  filter(date >= as.Date("2020-02-24")) %>%
  filter(date <= as.Date("2020-04-05"))

# add within-group mean of time-varying predictors
dta_mean <- dta %>%
  group_by(canton_codes) %>%
  arrange(date,.by_group=TRUE) %>%
  dplyr::summarize(across( contains( c("trips", "t_num") ), mean, .names = "{col}_mean"))

dta <- full_join(dta, dta_mean, by = "canton_codes")
remove(dta_mean)

# load avg distance travelled
dta_dist <- read_csv(file=here::here("Data","MIP_data","Avg_travel_distances","Avg_daily_travel_distance_cantons.csv"))

# pre-processing
dta_dist$date <- as.Date(dta_dist$date)
dta_dist <- dta_dist %>% 
  filter(date >= as.Date("2020-02-24")) %>% 
  filter(date <= as.Date("2020-04-05"))
dta <- full_join(dta, dta_dist, by = c("date","canton_codes"))
remove(dta_dist)

# load canton adjacency matrix
source(here::here("/Code","spatial_weights.R"))
W <- canton_neighbors


#--------------------- Main Model -------------------------

nb_logtrend <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
  
  priors1 <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                            coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE")),
                  set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                  set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                  set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                    "wdFriday", "wdSaturday", "wdSunday")))

  model_formula <- paste(paste0(m,"_trips_2020"), paste(". + offset(log(canton_pop)) + (1|canton_codes) - canton_pop - canton_codes"), sep = " ~ ")
  model_formula <- brmsformula(model_formula, center = TRUE)
  
  nb_logtrend[[paste(m)]] <- brm( model_formula,
                                  family = negbinomial(link = "log", link_shape = "log"),
                                  data=dta %>% dplyr::select(!!paste0(m,"_trips_2020"),
                                                             canton_pop, wd, t_num_ln, t_num_ln_mean, canton_codes,
                                                             eth_ban_5, gath100, eth_closed_borders, eth_closed_schools, eth_closed_stores_bars),
                                  prior = priors1, 
                                  inits = 0, 
                                  chains = 4,
                                  iter = 4000,
                                  thin = 2, 
                                  cores = parallel::detectCores(), 
                                  control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                  seed = 12345 )
  
}


#--------------------- Robustness checks: Alternative time-effect specifications -------------------------


nb_week_logtrend <- list()
nb_sqtrend <- list()
trip_cat <- c("total", "train", "road", "highway", "commuter", "non_commuter")

for ( m in trip_cat ){
  
  priors1 <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                         coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE")),
               set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
               set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
               set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                 "wdFriday", "wdSaturday", "wdSunday")))
  
  priors2 <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                         coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE")),
               set_prior("normal(0, 1)", class = "b", coef = "t_num"),
               set_prior("normal(0, 5)", class = "b", coef = "t_num_mean"),
               set_prior("normal(0, 1)", class = "b", coef = "t_num_sq"),
               set_prior("normal(0, 5)", class = "b", coef = "t_num_sq_mean"),
               set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                 "wdFriday", "wdSaturday", "wdSunday")))
  
  model_formula <- paste(paste0(m,"_trips_2020"), paste(". + offset(log(canton_pop)) + (1|canton_codes) - canton_pop - canton_codes"), sep = " ~ ")
  model_formula <- brmsformula(model_formula, center = TRUE)
  
  nb_week_logtrend[[paste(m)]] <- brm( model_formula,
                                       family = negbinomial(link = "log", link_shape = "log"),
                                       data=dta %>% dplyr::select(!!paste0(m,"_trips_2020"),
                                                                  canton_pop, wd, week_f, t_num_ln, t_num_ln_mean, canton_codes,
                                                                  eth_ban_5, gath100, eth_closed_borders, eth_closed_schools, eth_closed_stores_bars),
                                       prior = priors1, 
                                       inits = 0, 
                                       chains = 4,
                                       iter = 4000,
                                       thin = 2, 
                                       cores = parallel::detectCores(), 
                                       control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                       seed = 12345 )
  
  nb_sqtrend[[paste(m)]] <- brm( model_formula,
                                 family = negbinomial(link = "log", link_shape = "log"),
                                 data=dta %>% dplyr::select(!!paste0(m,"_trips_2020"),
                                                            canton_pop, wd, t_num, t_num_mean, t_num_sq, t_num_sq_mean, canton_codes,
                                                            eth_ban_5, gath100, eth_closed_borders, eth_closed_schools, eth_closed_stores_bars),
                                 prior = priors2, 
                                 inits = 0, 
                                 chains = 4,
                                 iter = 4000,
                                 thin = 2, 
                                 cores = parallel::detectCores(), 
                                 control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                 seed = 12345 )
}


#--------------------- Robustness check: Main Model with average distance instead of trips -------------------------

# fit model
nb_logtrend_dist <- list()

priors_dist <- c( set_prior("normal(-0.25, 0.5)", class = "b", 
                              coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE")),
                  set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"),
                  set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"),
                  set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                 "wdFriday", "wdSaturday", "wdSunday")))
  
model_formula <- paste(paste0("avg_km_by_resident"), paste(". + offset(log(canton_pop)) + (1|canton_codes) - canton_pop - canton_codes"), sep = " ~ ")
model_formula <- brmsformula(model_formula, center = TRUE)
  
# lognormal model as distances are not counts
nb_logtrend_dist <- brm(model_formula,
                        family = lognormal(link = "identity", link_sigma = "identity"),
                        data=dta %>% dplyr::select(avg_km_by_resident,
                                                    canton_pop, wd, t_num_ln, t_num_ln_mean, canton_codes,
                                                    eth_ban_5, gath100, eth_closed_borders, eth_closed_schools, eth_closed_stores_bars),
                        prior = priors_dist, 
                        inits = 0, 
                        chains = 4,
                        iter = 4000,
                        thin = 2, 
                        cores = parallel::detectCores(), 
                        control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                        seed = 12345 )


#--------------------- Spatial Model -------------------------


# Spatial random effect via BYM2 component

trip_cat <- c("total")

for ( m in trip_cat ){
  
  spatial_priors <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                                coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE")),
                      set_prior("normal(1, 1)", class = "b", coef = "t_num_ln"), 
                      set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean"), 
                      set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                        "wdFriday", "wdSaturday", "wdSunday")),
                      set_prior("student_t(3,  0, 2.5)", class = "sdcar"))
  
  model_formula <- paste(paste0(m,"_trips_2020"), 
                         paste(". + offset(log(canton_pop)) - canton_pop - canton_codes + car(M = W, gr = canton_codes, type = 'bym2')"), 
                         sep = " ~ ")
  
  model_formula <- brmsformula(model_formula, center = TRUE)
  
  nb_logtrend_bym2[[paste(m)]] <- brm( model_formula,
                                       family = negbinomial(link = "log", link_shape = "log"),
                                       data=dta %>% dplyr::select(!!paste0(m,"_trips_2020"),
                                                                  canton_pop, wd, t_num_ln, t_num_ln_mean, canton_codes,
                                                                  eth_ban_5, gath100, eth_closed_borders, eth_closed_schools, eth_closed_stores_bars),
                                       data2 = list(W = W),
                                       prior = spatial_priors, 
                                       inits = 0, 
                                       chains = 4,
                                       warmup = 8000,
                                       iter = 10000,
                                       thin = 2, 
                                       cores = parallel::detectCores(), 
                                       control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                                       seed = 12345 )
}


#--------------------- Multivariate Model -------------------------


all_priors <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "totaltrips2020"),
                set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "commutertrips2020"),
                set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "noncommutertrips2020"),
                set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "traintrips2020"),
                set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "roadtrips2020"),
                set_prior("normal(-0.25, 0.25)", class = "b", 
                          coef = c("eth_ban_5TRUE", "gath100TRUE", "eth_closed_bordersTRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE"),
                          resp = "highwaytrips2020"),
                
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "totaltrips2020"),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "commutertrips2020"),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "noncommutertrips2020"),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "traintrips2020"),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "roadtrips2020"),
                set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "highwaytrips2020"),
                

                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "totaltrips2020"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "commutertrips2020"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "noncommutertrips2020"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "traintrips2020"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "roadtrips2020"),
                set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "highwaytrips2020"),
                
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'totaltrips2020'),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'commutertrips2020'),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'noncommutertrips2020'),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'traintrips2020'),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'roadtrips2020'),
                set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday", "wdFriday", "wdSaturday", "wdSunday"), resp = 'highwaytrips2020'),
                
                set_prior("lkj_corr_cholesky(2)", class = "cor", group = "canton_codes"))

alltrips_formula <- brmsformula( mvbind(total_trips_2020, 
                                        commuter_trips_2020, non_commuter_trips_2020, train_trips_2020, road_trips_2020, highway_trips_2020)
                                 ~ offset(log(canton_pop)) + wd + t_num_ln + t_num_ln_mean +
                                   eth_ban_5 + gath100 + eth_closed_borders + eth_closed_schools + eth_closed_stores_bars +
                                   (1 |c| canton_codes),
                                 family = negbinomial(link = "log", link_shape = "log") )

nb_all_logtrend <- brm( formula = alltrips_formula,
                        data = dta,
                        prior = all_priors, 
                        inits = 0, 
                        chains = 4,
                        iter = 4000,
                        thin = 2, 
                        cores = parallel::detectCores(), 
                        control = list(adapt_delta = 0.99999, max_treedepth = 15), 
                        seed = 12345 )



#--------------------- Plots for Main Model and Robustness Checks -------------------------

# See script "helper_functions.R" for the functions used below

mod_list <- c(nb_logtrend, nb_week_logtrend, nb_sqtrend, nb_logtrend_dist)
plotname <- c("did_traffic_nb_logtrend.pdf", "did_traffic_nb_week_logtrend.pdf",
              "did_traffic_nb_sqtrend.pdf", "did_traffic_nb_logtrend_dist.pdf")

for(i in 1:length(plotname) ){
  
  # number of models per specification (no. of trip categories less one as first is counted)
  mods <- length(trip_cat)-1
  
  # obtain model indices in lists
  mod_index <- (i+(i-1)*mods):((i+(i-1)*mods)+mods)
  
  # set model to obtain estimates from. 
  model_to_check <- mod_list[mod_index]
  
  # get model posterior estimates
  did_total <- did_est(model_to_check, "total")
  did_mode <- did_est(model_to_check, "mode")
  did_purpose <- did_est(model_to_check, "purpose")
  
  # plot
  plot_total <- plot_did(did_total, "total")
  plot_mode <- plot_did(did_mode, "mode")
  plot_purpose <- plot_did(did_purpose, "purpose")
  
  # plot all side-by-side
  plot <- grid.arrange(plot_total, plot_mode, plot_purpose, widths = c(2.45, 2, 2))
  
  ggsave(filename = paste0(outpath, "/", plotname[i]), plot = plot, width = 12, height = 4)
}


#--------------------- Plot for Spatial Model  -------------------------

mod_list <- nb_logtrend_bym2
plotname <- "did_traffic_total_spatial_nb_logtrend"

# set model to obtain estimates from. 
model_to_check <- mod_list

# get model posterior estimates
did_total <- did_est(model_to_check, "total")

ggsave(filename = paste0(outpath, "/", plotname), plot = did_total, width = 12, height = 4)


# -------------------- Plot for Multivariate model -------------------------


model_to_check <- nb_all_logtrend 

# get model posterior estimates
did_total_mv <- did_est_mv(model_to_check, "total")
did_mode_mv <- did_est_mv(model_to_check, "mode")
did_purpose_mv <- did_est_mv(model_to_check, "purpose")

# plot
plot_total_mv <- plot_did(did_total_mv, "total")
plot_mode_mv <- plot_did(did_mode_mv, "mode")
plot_purpose_mv <- plot_did(did_purpose_mv, "purpose")

# plot all side-by-side
plot <- grid.arrange(plot_total_mv, plot_mode_mv, plot_purpose_mv, widths = c(2.45, 2, 2))

ggsave(filename = paste0(outpath,"/did_traffic_allplots_nb_mv_logtrend.pdf"), plot = plot, width = 12, height = 4)


#--------------------- Model diagnostics -------------------------


color_scheme_set("blue")
dep_vars <- c("total", "road", "train", "highway", "commuter", "non_commuter")
policies <- c("Ban > 100", "Closed schools", "Closed venues", "Ban > 5", "Closed borders")
param_names <- c("gath100TRUE", "eth_closed_schoolsTRUE", "eth_closed_stores_barsTRUE", "eth_ban_5TRUE", "eth_closed_bordersTRUE")

# specify model to get diagnostics from
model_to_check <- nb_logtrend

# Posterior predictive plot for the mobility models
pp_plots <- list()
for ( m in 1:length(dep_vars) ) {
  mod_obj <- model_to_check[[paste(dep_vars[m])]]
  
  # posterior predictive plot
  pp_plots[[m]] <- pp_check(mod_obj) + ggtitle( paste(capitalize(dep_vars[m]), "trips", sep = " ") )
}
pdf(file = paste0(outpath, "traffic_pp_check_total.pdf"), width = 12, height = 1.75)
pp_check(model_to_check$total) + ggtitle( paste(capitalize(dep_vars[m]), "trips", sep = " ") )
dev.off()


# Trace & Density of policy measures effect on each mobility category
for( m in dep_vars){
  posterior_plots <- list()
  param_plots <- list()
  
  for ( c in 1:length(param_names) ) {
    mod_obj <- model_to_check[[paste(m)]]
    
    # posterior chains plot
    posterior_plots[c] <- plot(mod_obj, 
                               pars = paste0("b_", param_names[c]), 
                               fixed = TRUE) 
    posterior_plots[[c]]$bayesplots[[1]]$labels$x <- posterior_plots[[c]]$bayesplots[[2]]$labels$y <- c("")
    posterior_plots[[c]]$bayesplots[[1]] <- posterior_plots[[c]]$bayesplots[[1]] + ggtitle(paste(capitalize(m),"trips ~", policies[c], sep = " "))
    
    # estimates of all parameters except intercept
    param_plots[[c]] <- mcmc_plot(mod_obj, pars = c("wd","eth", "gath"))
  }
  
  # Trace & Density plot
  pdf(file = paste0(outpath, "traffic_trace_density_",m,".pdf"), width = 12, height = 15)
  grid.arrange(posterior_plots[[1]]$bayesplots[[1]], posterior_plots[[1]]$bayesplots[[2]], 
               posterior_plots[[2]]$bayesplots[[1]], posterior_plots[[2]]$bayesplots[[2]],
               posterior_plots[[3]]$bayesplots[[1]], posterior_plots[[3]]$bayesplots[[2]],
               posterior_plots[[4]]$bayesplots[[1]], posterior_plots[[4]]$bayesplots[[2]],
               posterior_plots[[5]]$bayesplots[[1]], posterior_plots[[5]]$bayesplots[[2]],
               nrow = 5, ncol = 2)
  dev.off()

  # ACF, Neff and Rhat plots 
  acf_plots <- list()
  neff_plots <- list()
  rhat_plots <- list()
  
  for ( c in 1:length(param_names) ) {  
    mod_obj <- model_to_check[[paste(m)]]
    
    # ACF for all model parameters
    acf_plot <- mcmc_acf( as.array(mod_obj, pars = paste0("b_", param_names[c]), fixed = TRUE), lags = 10, size = 1 )
    acf_plot[[1]]$Parameter <- paste(policies[c])
    acf_plots[[c]] <- acf_plot + ggtitle(paste(capitalize(m),"trips ~", policies[c], sep = " "))
    
    # Neff for all model parameters
    neff_plots[[c]] <- mcmc_neff( neff_ratio(mod_obj), size = 3 ) + ggtitle(paste(capitalize(m),"trips ~", policies[c], sep = " "))
    
    # Rhat for all model parameters
    rhat_plots[[c]] <- mcmc_rhat( rhat(mod_obj), size = 3  ) + ggtitle(paste(capitalize(m),"trips ~", policies[c], sep = " "))
  }
  
  # ACF plot
  pdf(file = paste0(outpath, "traffic_acf_",m,".pdf"), width = 12, height = 15)
  grid.arrange(acf_plots[[1]], acf_plots[[2]], acf_plots[[3]], acf_plots[[4]], acf_plots[[5]],
               ncol = 2)
  dev.off()

  # Neff plot
  pdf(file = paste0(outpath, "traffic_neff_",m,".pdf"), width = 12, height = 15)
  grid.arrange(neff_plots[[1]], neff_plots[[2]], neff_plots[[3]], neff_plots[[4]], neff_plots[[5]],
               ncol = 2)
  dev.off()
  
  # Rhat plot
  pdf(file = paste0(outpath, "traffic_rhat_",m,".pdf"), width = 12, height = 15)
  grid.arrange(rhat_plots[[1]], rhat_plots[[2]], rhat_plots[[3]], rhat_plots[[4]], rhat_plots[[5]],
               ncol = 2)
  dev.off()

  # Plot of estimated Pareto tail shape parameter k against observation indices to check influential observations
  pdf(file = paste0(outpath, "traffic_pareto_",m,".pdf"), width = 12, height = 15)
  par( mfrow = c(3,2), cex = 0.85 )
  for ( c in 1:length(param_names) ) {
    mod_obj <- model_to_check[[paste(m)]]
    x <- loo(mod_obj) 
    plot(x, diagnostic = "k", label_points = FALSE, moment_match = TRUE, main = paste(capitalize(m),"trips ~", policies[c], sep = " ") )
  }
  dev.off()

  # plot bivariate posterior distributions of parameters with added regression line
  mod_obj <- model_to_check$total
  
  pdf(file = paste0(outpath, "traffic_bivardist_",m,".pdf"), width = 12, height = 12)
  
  mcmc_pairs(mod_obj, regex_pars = c("eth", "gath"))
  dev.off()
}
