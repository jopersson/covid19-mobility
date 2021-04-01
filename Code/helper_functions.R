# load library
library(stringr)

# function to print last n characters in string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# get nth digit from multi-digit integer
nth_digit <- function(x,n){
  str_sub(x, n,n)
}

# opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# add a common legend for combined ggplots - from Roland on StackOverflow
get_legend <- function(a.gplot){tmp <- ggplot_gtable(ggplot_build(a.gplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)}

# symetric difference between lists
sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))

 
# exporting tables
library("texreg")
texreg2 <- function(l,
                    include.ci=FALSE,
                    table=FALSE,
                    include.rsquared = F,
                    include.adjrs = FALSE,
                    include.rmse=FALSE,
                    file=NULL,
                    ...) {
  print(screenreg(l,include.ci=include.ci,table=table,include.rsquared=include.rsquared,include.adjrs=include.adjrs,include.rmse=include.rmse,
                  digits=3,stars = c(0.001, 0.01, 0.05, 0.1),...))
  if (!is.null(file)) {
    texreg(l,include.ci=include.ci,table=table,include.rsquared=include.rsquared,include.adjrs=include.adjrs,include.rmse=include.rmse,file=file,
           use.packages=FALSE,booktabs=FALSE,dcolumn=FALSE,
           digits=3,stars = c(0.001, 0.01, 0.05, 0.1),symbol="\\dagger",...)
  }
}

# IHS transform
ihs <- function(x) log(x + sqrt(x^2 + 1))


# get value from dictionary based on key
get_dict_value <- function(key,dict)  dict[key]

# tidy fixest class
tidy_fixest <- function(m,se,cluster) {
  m <- m %>% 
    rownames_to_column()  %>%
    as_tibble() %>%
    rename(term = rowname, 
           estimate = Estimate,
           std.error = `Std. Error`, 
           statistic = `z value`,
           p.value = `Pr(>|z|)`, 
           conf.low = `2.5 %`, conf.high = `97.5 %` )  
  return(m)
}


did_est <- function(model, mobility){
  # Input:
  #   model: list of brm model objects. Should be one of the (non-multivariate) fitted models (e.g., "mod_logtrend")
  #   mobility: The category of mobility ("character") for which to get model estimates. Should be one of "total", "mode", or "purpose".
  # Output: list of posterior estimates.
  
  if( mobility == "total" ){
    tripvars <- "total"
  }
  else if( mobility == "mode" ){
    tripvars <- c("train","road","highway")
  }
  else if( mobility == "purpose" ){
    tripvars <- c("commuter","non_commuter")
  }
  else{
    return("Please specify 'mobility' as one of 'total', 'mode', or 'purpose.")
    tripvars <- NULL
  }
  
  est_list <- list()
  for ( m in tripvars ){
    posterior <- as.matrix(model[[paste(m)]])
    posterior <- mcmc_intervals(posterior, prob = 0.8, prob_outer = 0.95, point_est = "mean")
    if( m %in% "total" ){
      est_list[[paste(m)]] <- add_column(posterior$data, Trips = "Total")
    }
    else if( m %in% c("train","road","highway") ){
      est_list[[paste(m)]] <- add_column(posterior$data, Mode = capitalize(paste(m)) )
    }
    else if( m %in% c("commuter","non_commuter") ){
      est_list[[paste(m)]] <- add_column(posterior$data, Purpose = capitalize(paste(m)) )
    }
    else
      return("Please specify second argument as one of 'total', c('train','road','highway'), or c('commuter','non_commuter').")
  }
  return( est_list %>% bind_rows() )
}


# get posterior parameter estimates from multivariate mobility~policy DiD model (brms object)
did_est_mv <- function(model, mobility){
  # Input:
  #   model: brm model object ("list). Should be the fitted multivariate model (i.e., "mod_all_logtrend")
  #   mobility: The category of mobility ("character") for which to get model estimates. Should be one of "total", "mode", or "purpose".
  # Output: posterior estimates ("list").
  
  if( mobility == "total" ){
    tripvars <- "total"
  }
  else if( mobility == "mode" ){
    tripvars <- c("train","road","highway")
  }
  else if( mobility == "purpose" ){
    tripvars <- c("commuter","non_commuter")
  }
  else{
    return("Please specify 'mobility' as one of 'total', 'mode', or 'purpose.")
    tripvars <- NULL
  }
  
  est_list <- list()
  
  posterior <- as.matrix(model)
  posterior <- mcmc_intervals(posterior, prob = 0.8, prob_outer = 0.95, point_est = "mean")
  posterior <- posterior$data
  
  for ( m in tripvars ){
    if( m == "total" ){
      post_est <- posterior %>% 
        filter( grepl("_total", as.character(posterior$parameter)) )
      est_list[[paste(m)]] <- add_column(post_est, Trips = capitalize(paste(m)) )  %>%
        mutate(parameter = str_replace(parameter, "totaltrips2020_", ""))
    }
    else if( m == "train" ){
      post_est <- posterior %>% 
        filter( grepl("_train", as.character(posterior$parameter)) )
      est_list[[paste(m)]] <- add_column(post_est, Mode = capitalize(paste(m)) )  %>%
        mutate(parameter = str_replace(parameter, "traintrips2020_", ""))
    }
    else if( m == "road" ){
      post_est <- posterior %>% 
        filter( grepl("_road", as.character(posterior$parameter)) )
      est_list[[paste(m)]] <- add_column(post_est, Mode = capitalize(paste(m)) )  %>%
        mutate(parameter = str_replace(parameter, "roadtrips2020_", ""))
    }
    else if( m == "highway" ){
      post_est <- posterior %>% 
        filter( grepl("_highway", as.character(posterior$parameter)) )
      est_list[[paste(m)]] <- add_column(post_est, Mode = capitalize(paste(m)) )  %>%
        mutate(parameter = str_replace(parameter, "highwaytrips2020_", ""))
    }
    else if( m == "commuter" ){
      post_est <- posterior %>% 
        filter( grepl("_commuter", as.character(posterior$parameter)) )  %>%
        mutate(parameter = str_replace(parameter, "commutertrips2020_", ""))
      est_list[[paste(m)]] <- add_column(post_est, Purpose = capitalize(paste(m)) )
    }
    else if( m == "non_commuter" ){
      post_est <- posterior %>% 
        filter( grepl("_noncommuter", as.character(posterior$parameter)) )  %>%
        mutate(parameter = str_replace(parameter, "noncommutertrips2020_", ""))
      est_list[[paste(m)]] <- add_column(post_est, Purpose = capitalize(paste(m)) )
    }
    else
      return("Please specify second argument as one of 'total', c('train','road','highway'), or c('commuter','non_commuter').")
  }
  return( est_list %>% bind_rows() )
}


# get posterior parameter estimates from covid~mobility model (brms object)
pred_est <- function(model, mobility, lags){
  # Input:
  #   model: brm model object ("list"). Should be the fitted multivariate model (i.e., "mod_bag_trend")
  #   mobility: The category of mobility ("character") for which to get model estimates. Should be one of "total", "mode", or "purpose".
  #   lags: The lag sequence ("integer") for which to get estimates. Should be "range_lags" or "range_lags_long" depending on for how many lags the model was fitted.
  # Output: Posterior estimates ("list").
  
  if( mobility == "total" ){
    tripvars <- "total"
  }
  else if( mobility == "mode" ){
    tripvars <- c("train","road","highway")
  }
  else if( mobility == "purpose" ){
    tripvars <- c("commuter","non_commuter")
  }
  else{
    return("Please specify 'mobility' as one of 'total', 'mode', or 'purpose.")
    tripvars <- NULL
  }
  
  est_list <- list()
  for ( m in tripvars ){
    for( s in lags ){
      posterior <- -as.matrix(model[[paste(m,s)]])
      posterior <- mcmc_intervals(posterior, prob = 0.8, prob_outer = 0.95, point_est = "mean")
      est_list[[paste(m,s)]] <- posterior$data
    }
  }
  return( est_list %>% bind_rows() )
}


sem_est <- function(model, mobility, lags){
  # Input:
  #   model: brm model object ("list"). Should be one of "direct_summary", "indirect_summary", or "total_summary"
  #   mobility: The category of mobility ("character") for which to get model estimates. May be one of "total", "mode", or "purpose", but is only implemented for "total" since the SEM is only fitted for total trips.
  #   lags: The lag sequence ("integer") for which to get estimates. Should be "range_lags".
  # Output: Posterior estimates ("list").
  
  if( mobility == "total" ){
    tripvars <- "total"
  }
  else if( mobility == "mode" ){
    tripvars <- c("train","road","highway")
  }
  else if( mobility == "purpose" ){
    tripvars <- c("commuter","non_commuter")
  }
  else{
    return("Please specify 'mobility' as one of 'total', 'mode', or 'purpose.")
    tripvars <- NULL
  }
  
  est_list <- list()
  for ( m in tripvars ){
    for( s in lags ){
      est_list[[paste(m,s)]] <- as_tibble(cbind(model[[paste(m,s)]], Lag=paste0(s,"th"), Trips=capitalize(paste(m))))
    }
  }
  return( est_list %>% bind_rows() )
}


# ggplot for mobility~policy models
plot_did <- function(estimates, mobility){
  # Input:
  #   estimates: Posterior estimates from brm model object. Should be an object returned by function "did_est" ("list").
  #   mobility: The category of mobility ("character") whose model estimates to plot. Should be one of "total", "mode", or "purpose".
  # Output: ggplot object.
  
  if(mobility == "total"){
    # tidy & plot
    estimates <- estimates  %>% 
      filter(str_detect(parameter, "eth") | str_detect(parameter, "gath") ) %>%
      mutate(parameter = str_replace(parameter, "TRUE", "")) %>%
      mutate(parameter = str_replace(parameter, "b_", "")) %>%
      arrange(Trips) %>%
      
      # calculate percentage effect and credible intervals
      mutate(outer_lb = 100*(exp(ll)-1)) %>%
      mutate(inner_lb = 100*(exp(l)-1)) %>%
      mutate(est = 100*(exp(m)-1)) %>%
      mutate(inner_ub = 100*(exp(h)-1)) %>%
      mutate(outer_ub = 100*(exp(hh)-1)) %>%
      
      # add explicit NA; otherwise facet_wrap add NA plot
      group_by(parameter, Trips) %>%
      ungroup() %>%
      
      # renaming
      mutate(term = case_when( 
        parameter == "gath100" ~ "Ban > 100",
        parameter == "eth_ban_5" ~ "Ban > 5",
        parameter == "eth_closed_borders" ~ "Closed borders",
        parameter == "eth_closed_schools" ~ "Closed schools",
        parameter == "eth_closed_stores_bars" ~ "Closed venues")) %>% 
      mutate(Order = case_when( 
        term == "Ban > 100" ~ 1,
        term == "Closed schools" ~ 2,
        term == "Closed venues" ~ 3,
        term == "Ban > 5" ~ 4,
        term == "Closed borders" ~ 5
      ))
    cols <- c("Total" = "#D34F53")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(term, -Order), y = est, ymin = outer_lb, ymax = outer_ub, color = Trips), 
                      size = 0.5,
                      position=position_dodge(width = 0.35) ) +
      # inner CI
      geom_linerange(aes(x = term, ymin = inner_lb, ymax = inner_ub, color = Trips), 
                     size = 1.25,
                     position=position_dodge(width = 0.35)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(legend.position="top") + # supress y-axis
      coord_flip() +
      ggtitle("") +
      labs(y = "Effect size (%)", x = "")
    plot
  } 
  else if(mobility == "mode"){
    # tidy & plot
    estimates <- estimates  %>% 
      filter(str_detect(parameter, "eth") | str_detect(parameter, "gath") ) %>%
      mutate(parameter = str_replace(parameter, "TRUE", "")) %>%
      mutate(parameter = str_replace(parameter, "b_", "")) %>%
      arrange(Mode) %>%
    
      # calculate percentage effect and credible intervals
      mutate(outer_lb = 100*(exp(ll)-1)) %>%
      mutate(inner_lb = 100*(exp(l)-1)) %>%
      mutate(est = 100*(exp(m)-1)) %>%
      mutate(inner_ub = 100*(exp(h)-1)) %>%
      mutate(outer_ub = 100*(exp(hh)-1))  %>%
      
      # add explicit NA; otherwise facet_wrap add NA plot
      group_by(parameter, Mode) %>%
      ungroup() %>%
      
      # renaming
      mutate(term = case_when( 
        parameter == "gath100" ~ "Ban > 100",
        parameter == "eth_ban_5" ~ "Ban > 5",
        parameter == "eth_closed_borders" ~ "Closed borders",
        parameter == "eth_closed_schools" ~ "Closed schools",
        parameter == "eth_closed_stores_bars" ~ "Closed venues")) %>%
      mutate(Order = case_when( 
        term == "Ban > 100" ~ 1,
        term == "Closed schools" ~ 2,
        term == "Closed venues" ~ 3,
        term == "Ban > 5" ~ 4,
        term == "Closed borders" ~ 5))
    cols <- c("Train" = "#E5921A", "Road" = "#73C2C0", "Highway" = "#884C6E")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(term, -Order), y = est, ymin = outer_lb, ymax = outer_ub, color = Mode),                  
                      size = 0.5,
                      position=position_dodge(width = 0.5)) +
      # inner CI
      geom_linerange(aes(x = term, ymin = inner_lb, ymax = inner_ub, color = Mode),                  
                     size = 1.25,
                     position=position_dodge(width = 0.5)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(axis.text.y=element_blank(), legend.position="top") + # supress y-axis
      coord_flip() +
      ggtitle("") +
      labs(y = "Effect size (%)", x = "")
    plot
  }
  else if(mobility == "purpose"){
    ### tidy & plot
    estimates <- estimates  %>% 
      filter(str_detect(parameter, "eth") | str_detect(parameter, "gath") ) %>%
      mutate(parameter = str_replace(parameter, "TRUE", "")) %>%
      mutate(parameter = str_replace(parameter, "b_", "")) %>%
      arrange(Purpose) %>%
      
      # Rename Non_commuter to Non-commuter
      mutate(Purpose = case_when(Purpose == 'Commuter' ~ 'Commuter',
                                 Purpose == 'Non_commuter' ~ 'Non-commuter',
                                 Purpose == 'Noncommuter' ~ 'Non-commuter')) %>%

      # calculate percentage effect and credible intervals
      mutate(outer_lb = 100*(exp(ll)-1)) %>%
      mutate(inner_lb = 100*(exp(l)-1)) %>%
      mutate(est = 100*(exp(m)-1)) %>%
      mutate(inner_ub = 100*(exp(h)-1)) %>%
      mutate(outer_ub = 100*(exp(hh)-1)) %>%
      
      # add explicit NA; otherwise facet_wrap add NA plot
      group_by(parameter, Purpose) %>%
      ungroup() %>%
      
      # renaming
      mutate(term = case_when( 
        parameter == "gath100" ~ "Ban > 100",
        parameter == "eth_ban_5" ~ "Ban > 5",
        parameter == "eth_closed_borders" ~ "Closed borders",
        parameter == "eth_closed_schools" ~ "Closed schools",
        parameter == "eth_closed_stores_bars" ~ "Closed venues")) %>%
      mutate(Order = case_when( 
        term == "Ban > 100" ~ 1,
        term == "Closed schools" ~ 2,
        term == "Closed venues" ~ 3,
        term == "Ban > 5" ~ 4,
        term == "Closed borders" ~ 5))
    cols <- c("Commuter" = "#008694", "Non-commuter" = "#0E5B90")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(term, -Order), y = est, ymin = outer_lb, ymax = outer_ub, color = Purpose),                  
                      size = 0.5,
                      position=position_dodge(width = 0.5)) +
      # inner CI
      geom_linerange(aes(x = term, ymin = inner_lb, ymax = inner_ub, color = Purpose),                  
                     size = 1.25,
                     position=position_dodge(width = 0.5)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(axis.text.y=element_blank(), legend.position="top") + # supress y-axis
      coord_flip() +
      ggtitle("") +
      labs(y = "Effect size (%)", x = "")
    plot
  }
  else
    return("Please specify second argument as one of 'total', 'mode', 'purpose'.")
}


# ggplot for covid~mobility models
plot_pred <- function(estimates, mobility){
  # Input:
  #   estimates: Posterior estimates from brm model object. Should be an object returned by function "pred_est" ("list").
  #   mobility: The category of mobility ("character") whose model estimates to plot. Should be one of "total", "mode", or "purpose".
  # Output: ggplot object.
  
  ## tidy & plot
  estimates <- estimates  %>% 
    filter(str_detect(parameter, "lag") ) %>%
    filter(str_detect(parameter, "_mean", negate = TRUE) )
    
  if(sum(grepl('lag20', estimates$parameter)) == 0){
    
    estimates <- estimates  %>% 
    # label mobility variables with their lags
    mutate(Lag = case_when( 
      str_detect(parameter, "lag7") ~ "7th",
      str_detect(parameter, "lag8") ~ "8th",
      str_detect(parameter, "lag9") ~ "9th",
      str_detect(parameter, "lag10") ~ "10th",
      str_detect(parameter, "lag11") ~ "11th",
      str_detect(parameter, "lag12") ~ "12th",
      str_detect(parameter, "lag13") ~ "13th")) %>% 
        
    # order lags correctly
    mutate(Order = case_when( 
      Lag == "7th" ~ 1,
      Lag == "8th" ~ 2,
      Lag == "9th" ~ 3,
      Lag == "10th" ~ 4,
      Lag == "11th" ~ 5,
      Lag == "12th" ~ 6,
      Lag == "13th" ~ 7))
    } else {
      
      estimates <- estimates  %>% 
      # label mobility variables with their lags
      mutate(Lag = case_when( 
        str_detect(parameter, "lag10") ~ "10th",
        str_detect(parameter, "lag11") ~ "11th",
        str_detect(parameter, "lag12") ~ "12th",
        str_detect(parameter, "lag13") ~ "13th",
        str_detect(parameter, "lag14") ~ "14th",
        str_detect(parameter, "lag15") ~ "15th",
        str_detect(parameter, "lag16") ~ "16th",
        str_detect(parameter, "lag17") ~ "17th",
        str_detect(parameter, "lag18") ~ "18th",
        str_detect(parameter, "lag19") ~ "19th",
        str_detect(parameter, "lag20") ~ "20th")) %>%
        
        # order lags correctly
        mutate(Order = case_when( 
          Lag == "10th" ~ 1,
          Lag == "11th" ~ 2,
          Lag == "12th" ~ 3,
          Lag == "13th" ~ 4,
          Lag == "14th" ~ 5,
          Lag == "15th" ~ 6,
          Lag == "16th" ~ 7,
          Lag == "17th" ~ 8,
          Lag == "18th" ~ 9,
          Lag == "19th" ~ 10,
          Lag == "20th" ~ 11))
  }
  
  if(mobility == "total"){
    estimates <- estimates  %>%
      # label mobility variables with their modes
      mutate(Trips = case_when( 
      str_detect(parameter, "total") ~ "Total",
      str_detect(parameter, "total") ~ "Total",
      str_detect(parameter, "total") ~ "Total")) %>% 
      # sort
      arrange(Order)
      cols <- c("Total" = "#D34F53")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(Lag, -Order), y = m, ymin = ll, ymax = hh, color = Trips),
                      size = 0.5,
                      position=position_dodge(width = 0.5)) +
      # inner CI
      geom_linerange(aes(x = reorder(Lag, -Order), ymin = l, ymax = h, color = Trips), 
                     size = 1.25,
                     position=position_dodge(width = 0.5)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(legend.position="top") +
      coord_flip() +
      ggtitle("") +
      labs(y = "Predicted change (%)", x = "Day ahead")
    
    return(plot)
  }
  else if(mobility == "mode"){
    ## tidy & plot
    estimates <- estimates  %>% 
      # label mobility variables with their modes
      mutate(Mode = case_when( 
        str_detect(parameter, "train") ~ "Train",
        str_detect(parameter, "road") ~ "Road",
        str_detect(parameter, "highway") ~ "Highway")) %>%
      # sort
      arrange(Order)
    cols <- c("Train" = "#E5921A", "Road" = "#73C2C0", "Highway" = "#884C6E")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(Lag, -Order), y = m, ymin = ll, ymax = hh, color = Mode),                  
                      size = 0.5,
                      position=position_dodge(width = 0.5)) +
      # inner CI
      geom_linerange(aes(x = Lag, ymin = l, ymax = h, color = Mode),                  
                     size = 1.25,
                     position=position_dodge(width = 0.5)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(axis.text.y=element_blank(), legend.position="top") +
      coord_flip() +
      ggtitle("") +
      labs(y = "Predicted change (%)", x = "")
    
    return(plot)
  }
  else if(mobility == "purpose"){
    ## tidy & plot
    estimates <- estimates  %>% 
      # label mobility variables with their modes
      mutate(Purpose = case_when( 
        str_detect(parameter, "non_commuter") ~ "Non-commuter",
        str_detect(parameter, "commuter") ~ "Commuter")) %>%
      # sort
      arrange(Order)
    cols <- c("Commuter" = "#008694", "Non-commuter" = "#0E5B90")
    plot <- estimates %>%
      ggplot() +
      # outer CI
      geom_pointrange(aes(x = reorder(Lag, -Order), y = m, ymin = ll, ymax = hh, color = Purpose),                  
                      size = 0.5,
                      position=position_dodge(width = 0.5)) +
      # inner CI
      geom_linerange(aes(x = Lag, ymin = l, ymax = h, color = Purpose),                  
                     size = 1.25,
                     position=position_dodge(width = 0.5)) +
      scale_color_manual(values = cols) +
      theme_bw() +
      geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
      theme(axis.text.y=element_blank(), legend.position="top") +
      coord_flip() +
      ggtitle("") +
      labs(y = "Predicted change (%)", x = "")
    
    return(plot)
  }
  else
    return("Please specify second argument as one of 'total', 'mode', 'purpose'.")
}


# ggplot for SEM model
plot_sem <- function(estimates, effect){
  # Input:
  #   estimates: Posterior estimates from brm model object. Should be an object returned by function "sem_est" ("list").
  #   effect: The mediation effect to obtain ("character"). Should be one of "direct", "indirect", or "total".
  # Output: ggplot object.
  
  # Order lags as factors
  estimates$Lag <- as.factor(estimates$Lag)
  
  if( effect == "indirect" ){
    # Rename variables
    estimates <- estimates  %>% 
      mutate(parameter = str_replace(parameter, "TRUE", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag7", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag8", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag9", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag10", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag11", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag12", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag13", "")) %>%
      mutate(parameter = str_replace(parameter, "lag7_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag8_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag9_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag10_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag11_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag12_", "")) %>%
      mutate(parameter = str_replace(parameter, "lag13_", "")) %>%
      mutate(parameter = str_replace(parameter, "b_totaltrips2020", "")) %>%
      arrange(Trips) 
  } 
  else {
    # Rename variables
    estimates <- estimates  %>% 
      mutate(parameter = str_replace(parameter, "TRUE", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag7", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag8", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag9", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag10", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag11", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag12", "")) %>%
      mutate(parameter = str_replace(parameter, "_lag13", "")) %>%
      mutate(parameter = str_replace(parameter, "b_bagfalle_", "")) %>%
      arrange(Trips)
  } 
  estimates <- estimates  %>% 
    # renaming
    mutate(term = case_when( 
      parameter == "gath100" ~ "Ban > 100",
      parameter == "eth_ban_5" ~ "Ban > 5",
      parameter == "eth_closed_borders" ~ "Closed borders",
      parameter == "eth_closed_schools" ~ "Closed schools",
      parameter == "eth_closed_stores_bars" ~ "Closed venues")) %>%
    mutate(Order = case_when( 
      term == "Ban > 100" ~ 1,
      term == "Closed schools" ~ 2,
      term == "Closed venues" ~ 3,
      term == "Ban > 5" ~ 4,
      term == "Closed borders" ~ 5)) %>%
    mutate(LagOrder = case_when( 
      Lag == "7th" ~ 7,
      Lag == "8th" ~ 6,
      Lag == "9th" ~ 5,
      Lag == "10th" ~ 4,
      Lag == "11th" ~ 3,
      Lag == "12th" ~ 2,
      Lag == "13th" ~ 1))
  # Filter out the policy variables
  estimates_ban100 <- estimates  %>% 
    filter(term == "Ban > 100")
  estimates_schools <- estimates  %>% 
    filter(term == "Closed schools")
  estimates_venues <- estimates  %>% 
    filter(term == "Closed venues")
  estimates_ban5 <- estimates  %>% 
    filter(term == "Ban > 5")
  estimates_borders <- estimates  %>% 
    filter(term == "Closed borders")
  # plot Ban > 100
  plot_ban100 <- estimates_ban100 %>%
    ggplot() +
    # outer CI
    geom_pointrange(aes(x = reorder(Lag, LagOrder), y = m, ymin = ll, ymax = hh), 
                    color = "#D34F53", size = 0.5,
                    position=position_dodge(width = 1) ) +
    # inner CI
    geom_linerange(aes(x = Lag, ymin = l, ymax = h), 
                   color = "#D34F53", size = 1.25,
                   position=position_dodge(width = 1)) +
    scale_x_discrete(expand=c(0.11, 0)) +
    theme_bw() +
    geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
    coord_flip() +
    ggtitle(paste0(estimates_ban100$term)[1]) +
    labs(y = "Effect size (%)", x = "Day ahead") +
    expand_limits(y = c(min(estimates$ll), max(estimates$hh)))
  # plot Schools
  plot_schools <- estimates_schools %>%
    ggplot() +
    # outer CI
    geom_pointrange(aes(x = reorder(Lag, LagOrder), y = m, ymin = ll, ymax = hh), 
                    color = "#D34F53", size = 0.5,
                    position=position_dodge(width = 1) ) +
    # inner CI
    geom_linerange(aes(x = Lag, ymin = l, ymax = h), 
                   color = "#D34F53", size = 1.25,
                   position=position_dodge(width = 1)) +
    scale_x_discrete(expand=c(0.11, 0)) +
    theme_bw() +
    theme(axis.text.y=element_blank(), legend.position="top") +
    geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
    coord_flip() +
    ggtitle(paste0(estimates_schools$term)[1]) +
    labs(y = "Effect size (%)", x = "") +
    expand_limits(y = c(min(estimates$ll), max(estimates$hh)))
  # plot Venues
  plot_venues <- estimates_venues %>%
    ggplot() +
    # outer CI
    geom_pointrange(aes(x = reorder(Lag, LagOrder), y = m, ymin = ll, ymax = hh), 
                    color = "#D34F53", size = 0.5,
                    position=position_dodge(width = 1) ) +
    # inner CI
    geom_linerange(aes(x = Lag, ymin = l, ymax = h), 
                   color = "#D34F53", size = 1.25,
                   position=position_dodge(width = 1)) +
    scale_x_discrete(expand=c(0.11, 0)) +
    theme_bw() +
    theme(axis.text.y=element_blank(), legend.position="top") +
    geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
    coord_flip() +
    ggtitle(paste0(estimates_venues$term)[1]) +
    labs(y = "Effect size (%)", x = "") +
    expand_limits(y = c(min(estimates$ll), max(estimates$hh)))
  # plot Ban > 5
  plot_ban5 <- estimates_ban5 %>%
    ggplot() +
    # outer CI
    geom_pointrange(aes(x = reorder(Lag, LagOrder), y = m, ymin = ll, ymax = hh), 
                    color = "#D34F53", size = 0.5,
                    position=position_dodge(width = 1) ) +
    # inner CI
    geom_linerange(aes(x = Lag, ymin = l, ymax = h), 
                   color = "#D34F53", size = 1.25,
                   position=position_dodge(width = 1)) +
    scale_x_discrete(expand=c(0.11, 0)) +
    theme_bw() +
    theme(axis.text.y=element_blank(), legend.position="top") +
    geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
    coord_flip() +
    ggtitle(paste0(estimates_ban5$term)[1]) +
    labs(y = "Effect size (%)", x = "") +
    expand_limits(y = c(min(estimates$ll), max(estimates$hh)))
  # plot Ban > 5
  plot_borders <- estimates_borders %>%
    ggplot() +
    # outer CI
    geom_pointrange(aes(x = reorder(Lag, LagOrder), y = m, ymin = ll, ymax = hh), 
                    color = "#D34F53", size = 0.5,
                    position=position_dodge(width = 1) ) +
    # inner CI
    geom_linerange(aes(x = Lag, ymin = l, ymax = h), 
                   color = "#D34F53", size = 1.25,
                   position=position_dodge(width = 1)) +
    scale_x_discrete(expand=c(0.11, 0)) +
    theme_bw() +
    theme(axis.text.y=element_blank(), legend.position="top") +
    geom_hline(yintercept = 0, size = 0.4, linetype = 2) +
    coord_flip() +
    ggtitle(paste0(estimates_borders$term)[1]) +
    labs(y = "Effect size (%)", x = "") +
    expand_limits(y = c(min(estimates$ll), max(estimates$hh)))
  
  plot <- grid.arrange(plot_ban100, plot_schools, plot_venues, plot_ban5, plot_borders,
                       nrow = 1,
                       top = textGrob(paste0(capitalize(effect)," effect"), gp = gpar(fontface = 1, fontsize = 18)))
  plot
}

