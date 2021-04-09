library("rstanarm")
library("brms")
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
library("haven")
library("bayesplot")
library("grid")
library("gridExtra")
library("Hmisc")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(12345)

rm(list=ls())

source(here::here("Code","helper_functions.R"))

outpath <- here::here("Plots/mediation_analysis/")

dta <- read_csv(file=here::here("Data","Merged_panel_data.csv"))


#--------------------- Data preparation -------------------------

# variable pre-processing
dta <- dta %>%
  mutate(canton_codes = as.factor(canton_codes)) %>%
  mutate_at(vars(contains("trips")), as.numeric ) %>%
  mutate(t_num = as.numeric(t)) %>%
  mutate(t_num = if_else(t_num<0, 0, t_num)) %>%
  mutate(t_num_sq = t_num^2,
         t_num_ln = log(1+t_num),
         t_f = as.factor(t_num),
         wd = as.factor(wd),
         date_f = as.factor(date),
         week_f = as.factor(strftime(date, format = "%V"))) %>%
  within(dta, wd <- relevel(wd, ref = "Monday")) # Set monday as reference weekday

### create leads of variables
dta <- dta %>% 
  group_by(canton_codes) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate_at(vars(contains("trips"), 
                 contains("eth"), 
                 contains("gath"), 
                 contains("t_num"), 
                 contains("t_num_ln"),
                 contains("t_num_sq"), 
                 contains("t_f"),
                 contains("wd"), 
                 contains("date_f"),
                 contains("weekf_f")),
            list(lag0 = ~ ., 
                 lag7=~dplyr::lag(.,n=7),
                 lag8=~dplyr::lag(.,n=8),
                 lag9=~dplyr::lag(.,n=9),
                 lag10=~dplyr::lag(.,n=10),
                 lag11=~dplyr::lag(.,n=11),
                 lag12=~dplyr::lag(.,n=12),
                 lag13=~dplyr::lag(.,n=13)
            )) %>% 
  ungroup()

# subset time frame
dta <- dta %>% 
  filter(date >= as.Date("2020-02-24")) %>% # Monday to Sunday
  filter(date <= as.Date("2020-04-05")) 

dta <- dta %>% 
  filter(t_num>0) # only after 1st case

# remove some policies since we can't identify all of them
dta <- dta %>% dplyr::select(-contains("ban_30"),
                             -contains("museum"),
                             -contains("hygiene"),
                             -contains("hair"),
                             -contains("test"),
                             -contains("eth_ban_1000"))

# add within-group mean of time-varying predictors
dta_mean <- dta %>%
  group_by(canton_codes) %>%
  arrange(date,.by_group=TRUE) %>%
  dplyr::summarize(across( contains( c("trips", "t_num") ), mean, .names = "{col}_mean"))

dta <- full_join(dta, dta_mean, by = "canton_codes")
remove(dta_mean)


#--------------------- Structural equation model -------------------------

range_lags <- 7:13 
nb_sem_logtrend <- list() 

direct_summary <- list()
mediator_summary <- list()
indirect_summary <- list()
total_summary <- list()
prop_med_summary <- list()

direct_post <- list()
mediator_post <- list()
treat_to_mediator <- list()
indirect_post <- list()
total_post <- list()
prop_med_post <- list()

m = "total"
for ( s in range_lags ) {
  
  priors <- c(set_prior("normal(-0.25, 0.25)", class = "b", 
                        coef = c(paste0("eth_ban_5_lag",s,"TRUE"), paste0("gath100_lag",s,"TRUE"),
                                 paste0("eth_closed_borders_lag",s,"TRUE"), paste0("eth_closed_schools_lag",s,"TRUE"), paste0("eth_closed_stores_bars_lag",s,"TRUE")),
                        resp = paste0("totaltrips2020lag",s) ),
              set_prior("normal(-0.25, 0.125)", class = "b", 
                        coef = c(paste0("eth_ban_5_lag",s,"TRUE"), paste0("gath100_lag",s,"TRUE"),
                                 paste0("eth_closed_borders_lag",s,"TRUE"), paste0("eth_closed_schools_lag",s,"TRUE"), paste0("eth_closed_stores_bars_lag",s,"TRUE")),
                        resp = "bagfalle"), 
              set_prior("normal(1, 1)", class = "b", coef = paste0("t_num_ln_lag",s), resp = paste0("totaltrips2020lag",s) ),
              set_prior("normal(0, 5)", class = "b", coef = paste0("t_num_ln_lag",s,"_mean"), resp = paste0("totaltrips2020lag",s) ),
              set_prior("normal(1, 1)", class = "b", coef = "t_num_ln", resp = "bagfalle" ),
              set_prior("normal(0, 5)", class = "b", coef = "t_num_ln_mean", resp = "bagfalle" ),
              set_prior("normal(0.5, 0.125)", class = "b", coef = paste0("ln_total_trips_2020_lag",s), resp = "bagfalle" ),
              set_prior("normal(0, 5)", class = "b", coef = paste0("ln_total_trips_2020_lag",s,"_mean"), resp = "bagfalle" ),
              set_prior("normal(0, 0.5)", class = "b", coef = c(paste0("wd_lag",s,"Tuesday"), paste0("wd_lag",s,"Wednesday"), paste0("wd_lag",s,"Thursday"),
                                                                paste0("wd_lag",s,"Friday"), paste0("wd_lag",s,"Saturday"), paste0("wd_lag",s,"Sunday")),
                        resp = paste0("totaltrips2020lag",s) ),
              set_prior("normal(0, 0.5)", class = "b", coef = c("wdTuesday", "wdWednesday", "wdThursday",
                                                                "wdFriday", "wdSaturday", "wdSunday" ),
                        resp = "bagfalle" ),
              set_prior("lkj_corr_cholesky(2)", class = "cor", group = "canton_codes")
  )
  
  mediator_depvar <- paste0(m,"_trips_2020_lag",s)
  mediator_predvar  <- c("(1 |c| canton_codes)", "offset(log(canton_pop))", 
                         paste0("wd_lag",s), paste0("t_num_ln_lag",s), paste0("t_num_ln_lag",s,"_mean"),
                         paste0("eth_ban_5_lag",s), paste0("gath100_lag",s), 
                         paste0("eth_closed_borders_lag",s), paste0("eth_closed_schools_lag",s), paste0("eth_closed_stores_bars_lag",s) )
  mediator_formula <- as.formula(paste(mediator_depvar, paste(mediator_predvar, collapse=" + "), sep=" ~ "))
  
  outcome_depvar <- "bag_falle"
  outcome_predvar  <- c("(1 |c| canton_codes)", "offset(log(canton_pop))", 
                        "wd", "t_num_ln", "t_num_ln_mean",
                        paste0("eth_ban_5_lag",s), paste0("gath100_lag",s), 
                        paste0("eth_closed_borders_lag",s), paste0("eth_closed_schools_lag",s), paste0("eth_closed_stores_bars_lag",s),
                        paste0("ln_",m,"_trips_2020_lag",s), paste0("ln_",m,"_trips_2020_lag",s,"_mean") )
  outcome_formula <- as.formula(paste(outcome_depvar, paste(outcome_predvar, collapse=" + "), sep=" ~ "))
  
  nb_sem_logtrend[[paste(m,s)]] <- brm(  mvbf(mediator_formula, outcome_formula, rescor = NULL),
                                      data = dta,
                                      family = negbinomial(link = "log", link_shape = "log"),
                                      prior = priors,
                                      inits = 0,
                                      chains = 4,
                                      iter = 4000,
                                      thin = 2,
                                      cores = parallel::detectCores(),
                                      control = list(adapt_delta = 0.99999, max_treedepth=15),
                                      seed = 12345)
  
  # direct effect estimates
  direct_post[[paste(m,s)]] <- posterior_samples(nb_sem_logtrend[[paste(m,s)]], pars = c(paste0("b_bagfalle_eth_ban_5_lag",s,"TRUE"), paste0("b_bagfalle_gath100_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_borders_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_schools_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_stores_bars_lag",s,"TRUE")))
  direct <- as.matrix(direct_post[[paste(m,s)]])
  direct <- 100*(exp(direct)-1) # transformation to multiplicative effects on natural scale
  direct <- mcmc_intervals(direct, pars = c(paste0("b_bagfalle_eth_ban_5_lag",s,"TRUE"), paste0("b_bagfalle_gath100_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_borders_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_schools_lag",s,"TRUE"), paste0("b_bagfalle_eth_closed_stores_bars_lag",s,"TRUE")),
                           prob = 0.8, prob_outer = 0.95, point_est = "mean")
  direct_summary[[paste(m,s)]] <- direct$data
  
  # mediator effect estimates
  mediator_post[[paste(m,s)]] <- posterior_samples(nb_sem_logtrend[[paste(m,s)]], pars = paste0("b_bagfalle_ln_",m,"_trips_2020_lag",s))[paste0("b_bagfalle_ln_",m,"_trips_2020_lag",s)]
  mediator <- as.matrix(mediator_post[[paste(m,s)]])
  mediator <- mcmc_intervals(mediator, pars = paste0("b_bagfalle_ln_",m,"_trips_2020_lag",s),
                             prob = 0.8, prob_outer = 0.95, point_est = "mean")
  mediator_summary[[paste(m,s)]] <- mediator$data
  rep_mediator_post <- as.matrix(data.frame(rep(mediator_post[[paste(m,s)]], 5))) # repeat mediator effect for product method of indirect effect
  
  # treatment effect on mediator estimates
  treat_to_mediator[[paste(m,s)]] <- posterior_samples(nb_sem_logtrend[[paste(m,s)]], c(paste0("b_",m,"trips2020lag",s,"_eth_ban_5_lag",s,"TRUE"), paste0("b_",m,"trips2020lag",s,"_gath100_lag",s,"TRUE"), paste0("b_",m,"trips2020lag",s,"_eth_closed_borders_lag",s,"TRUE"), paste0("b_",m,"trips2020lag",s,"_eth_closed_schools_lag",s,"TRUE"), paste0("b_",m,"trips2020lag",s,"_eth_closed_stores_bars_lag",s,"TRUE")))     
  treat_mediator <- as.matrix(treat_to_mediator[[paste(m,s)]])
  treat_mediator <- 100*(exp(treat_mediator)-1) # transformation to multiplicative effects on natural scale
  
  # indirect effect estimates
  indirect_post[[paste(m,s)]] <- treat_mediator * rep_mediator_post # product method
  indirect <- as.matrix(indirect_post[[paste(m,s)]])
  indirect <- mcmc_intervals(indirect, prob = 0.8, prob_outer = 0.95, point_est = "mean")
  indirect_summary[[paste(m,s)]] <- indirect$data
  
  # total effect estimates
  total_post[[paste(m,s)]] <- indirect_post[[paste(m,s)]] + 100*( exp(direct_post[[paste(m,s)]]) - 1 )
  total <- as.matrix(total_post[[paste(m,s)]])
  total <- mcmc_intervals(total, prob = 0.8, prob_outer = 0.95, point_est = "mean")
  total_summary[[paste(m,s)]] <- total$data
  
  # proportion of total effect mediated
  prop_med_post[[paste(m,s)]] <- indirect_post[[paste(m,s)]] / total_post[[paste(m,s)]]
  prop_med <- as.matrix(prop_med_post[[paste(m,s)]])
  prop_med <- mcmc_intervals(prop_med, prob = 0.8, prob_outer = 0.95, point_est = "mean")
  prop_med_summary[[paste(m,s)]] <- prop_med$data
}


# ------------------ Plot ---------------


# See script "helper_functions.R" for the functions used below

# get model posterior estimates
direct_effects   <- sem_est(direct_summary, "total", range_lags)
indirect_effects <- sem_est(indirect_summary, "total", range_lags)
total_effects    <- sem_est(total_summary, "total", range_lags)

# plot
plot_direct   <- plot_sem(direct_effects, "direct")
plot_indirect <- plot_sem(indirect_effects, "indirect")
plot_total    <- plot_sem(total_effects, "total")

# write pdfs
ggsave(filename = paste0(outpath, "/", "sem_direct_effects.pdf"), plot = plot_direct, width = 12, height = 3)
ggsave(filename = paste0(outpath, "/", "sem_indirect_effects.pdf"), plot = plot_indirect, width = 12, height = 3)
ggsave(filename = paste0(outpath, "/", "sem_total_effects.pdf"), plot = plot_total, width = 12, height = 3)


#--------------------- Model diagnostics -------------------------


# Check unconfoundedness assumption via correlation in mediator and outcome models' sampled predictive errors
m = "total"
cor_mean <- c()
cor_lb <- c()
cor_ub <- c()
cor_plot <- list()
par( mfrow = c(2,4), cex = 0.85, mar = c(5,4,2,1))
for( s in 7:13){
  cor_vec <- c()
  res_mediator <- residuals(nb_sem_logtrend[[paste(m,s)]], resp = paste0(m,"trips2020lag",s), method = "posterior_predict", type = "ordinary", summary = F)
  res_outcome <- residuals(nb_sem_logtrend[[paste(m,s)]], resp = "bagfalle", method = "posterior_predict", type = "ordinary", summary = F)
  for(k in 1:dim(res_mediator)[1]){
    cor_vec[k] <- cor(res_mediator[k,], res_outcome[k,], method = "pearson")
  }
  
  # density plot
  dens <- density(cor_vec)
  cor_plot <- plot(dens, lwd = 3, col = "gray50",
                   xlab = "Pearson correlation coefficient", main = paste0("Lag ", s) ); grid()
  
  q0.025 <- quantile(cor_vec, probs = 0.025) # lower 2.5% correlation quantile
  q0     <- quantile(cor_vec, probs = 0)     # min
  q0.975 <- quantile(cor_vec, probs = 0.975) # upper 2.5% correlation quantile
  q1     <- quantile(cor_vec, probs = 1)     # max
  
  # find index value of the quantiles
  x1 <- max(which(dens$x <= q0.025))
  x2 <- min(which(dens$x >= q0.975))  
  
  # add shaded regions for the quantiles to the plot
  with(dens, polygon(x=c(x[c(x1, x1:x2, x2)]), y= c(0, y[x1:x2], 0), col="gray80"))
  
  # summary statistics
  cor_mean[s-6] <- round(mean(cor_vec), 3) # mean correlation
  cor_lb[s-6] <- round(q0.025, 3) 
  cor_ub[s-6] <- round(q0.975, 3) 
}
