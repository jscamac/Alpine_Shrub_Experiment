# Summaries regression coefficients
summarise_coefficients <- function(model, params, quantiles = c(0.025,0.1,0.5,0.9, 0.975)) {
  samples <- rstan::extract(model$fit, pars=params)
  
  res <-lapply(samples, function(x) {
    if(is.matrix(x)) {
      df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      df$grp <- rownames(df)
      df
    }
    else {
      df <- cbind.data.frame(
        mean = mean(x),
        sd = sd(x),
        t(quantile(x, quantiles)))
      df
    }
  })
  res <-plyr::ldply(res, .id='parameter')
  if(nrow(res) > length(params)) {
    res$parameter <- factor(paste(res$parameter,res$grp, sep='_'), levels= rev(paste(res$parameter,res$grp, sep='_')))
    res <- dplyr::select(res,-grp)
  } else
    res$parameter <- factor(res$parameter, levels = rev(res$parameter))
  return(res)
}

# Summaries OTC model predictions
summarise_otc_model_predictions <- function(analysis, model, quantiles = c(0.025, 0.975)) {
  genus <- c('Asterolasia','Grevillea','Phebalium','Prostanthera')
  if(analysis =='non_tussock_growth') {
    samples <- rstan::extract(model$fit, pars=c('pred_height_ctl','pred_height_otc'))
    pred_var <- data.frame(model$stan_data['pred_year'])
    
    summary_df <-lapply(samples, function(x) {
      mn  <- melt(apply(x,2:3, mean))
      std <- melt(apply(x,2:3, sd))
      q <- dcast(melt(aperm(apply(x,2:3,quantile, quantiles),c(2,3,1))),
                 Var1 + Var2 ~ Var3, value.var = 'value')
      
      df <- arrange(merge(mn,std,by = c('Var1','Var2')) %>% merge(q, by = c('Var1','Var2')),Var2,Var1)[-1]
      names(df)[1:3] <- c('spp','mean','sd')
      df$spp <-factor(df$spp, labels = genus)
      cbind(pred_var,df)
    })
  }
  
  if(analysis == 'non_tussock_mortality') {
    samples <- rstan::extract(model$fit, pars=c('p_death_ctl','p_death_otc'))
    pred_var <- data.frame(treatment = as.factor(c(rep('ctl',4),rep('otc',4))))
    
    summary_df <-lapply(samples, function(x) {
      df <- cbind.data.frame(
        spp = genus,
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
    })
  }
  
  if(analysis == 'tussock_growth') {
    samples <- rstan::extract(model$fit, pars=c('r_ctl','r_otc'))
    pred_var <- data.frame(model$stan_data['sim_median_poadist'])
    
    summary_df <- lapply(samples, function(x) {
      df <- cbind.data.frame(
        spp = rep('Grevillea', nrow(pred_var)),
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      cbind(pred_var,df)
    })
  }
  
  if(analysis =='tussock_mortality') {
    
    samples <- rstan::extract(model$fit, pars=c('p_death_ctl','p_death_otc'))
    pred_var <- data.frame(model$stan_data['sim_median_poadist'])
    
    summary_df <- lapply(samples, function(x) {
      df <- cbind.data.frame(
        spp = rep('Grevillea', nrow(pred_var)),
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      cbind(pred_var,df)
    })
  }
  
  if(analysis =='gap_dynamics') {
    samples <- rstan::extract(model$fit, pars=c('preds_ctl','preds_otc'))
    pred_var <- data.frame(model$stan_data['pred_yr'])
    
    summary_df <-lapply(samples, function(x) {
      df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      cbind(pred_var,df)
    })
  }
  
  if(!analysis %in% c('non_tussock_growth',
                      'non_tussock_mortality',
                      'tussock_growth',
                      'tussock_mortality',
                      'gap_dynamics')) {
    stop("analysis can only be one of the following: 
         'non_tussock_growth','non_tussock_mortality',
         'tussock_growth', 'tussock_mortality', 'gap_dynamics")
  }

        res <- plyr::ldply(summary_df, .id='prediction')
        res$treatment <- as.factor(ifelse(grepl("otc", res$prediction, ignore.case = T), "otc", 
                          ifelse(grepl("ctl", res$prediction, ignore.case = T),"ctl", NA)))
  return(res)
}

# Extract mean predicted year hmax is reached
extract_hmax_year <- function(summarised_predictions) {
  summarised_predictions %>%
    mutate(mean = round(mean)) %>%
    group_by(treatment) %>%
    filter(spp=='Asterolasia' & mean==34 |
             spp =='Grevillea' & mean==67 |
             spp =='Phebalium' & mean==66 |
             spp =='Prostanthera' & mean==70) %>%
    droplevels() %>%
    ungroup() %>%
    group_by(spp, treatment) %>%
    filter(pred_year == min(pred_year)) %>%
    droplevels() %>%
    select(treatment, pred_year, spp, hmax=mean) %>%
    arrange(spp, treatment)
}

# Summaries predictions from density models
summarise_density_predictions <- function(model, species='Grevillea', quantiles = c(0.025, 0.975)) {
  if(species =='Grevillea') {
    samples <- rstan::extract(model$fit, 
                              pars = c('pred_count_unburnt', 'pred_count_burnt',
                                       'pred_count_severity', 'pred_count_altitude',
                                       'pred_count_twi', 'pred_count_adult_density'))
    
    pred_var <- model$stan_data[c('sim_severity','sim_altitude','sim_twi','sim_adult_den')]
  }
  if(species =='Asterolasia') {
    samples <- rstan::extract(model$fit, 
                              pars =c('pred_count_unburnt', 
                                      'pred_count_burnt', 'pred_count_severity', 
                                      'pred_count_altitude', 'pred_count_twi'))
    
    pred_var <- model$stan_data[c('sim_severity','sim_altitude','sim_twi')]
  }
  
  lookup <- list(p_absent_severity ='sim_severity',pred_count_severity ='sim_severity',
                 p_absent_altitude ='sim_altitude',pred_count_altitude ='sim_altitude',
                 p_absent_twi ='sim_twi',pred_count_twi ='sim_twi',
                 p_absent_adult_density ='sim_adult_den',pred_count_adult_density ='sim_adult_den')
  
  
  summary_df <- mapply(function(x, name) {
    if(length(dim(x))==2) {
      df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      cbind(pred_var[lookup[[name]]], df) 
    } else {
      cbind.data.frame(
        mean = mean(x),
        sd = sd(x),
        t(quantile(x,quantiles)))
    }
  }, x =samples, name =names(samples), SIMPLIFY = FALSE)
  
  pred_count_b_ub <- plyr::ldply(summary_df[c('pred_count_unburnt','pred_count_burnt')], .id='predictor')
  
  if(species =='Grevillea') {
    res <- c(list(burnt_unburnt = pred_count_b_ub, severity = summary_df$pred_count_severity,
           altitude = summary_df$pred_count_altitude, twi =summary_df$pred_count_twi,
           adult_density = summary_df$pred_count_adult_density))
  }
  
  if(species =='Asterolasia') {
    res <-  c(list(burnt_unburnt = pred_count_b_ub, severity = summary_df$pred_count_severity,
                   altitude = summary_df$pred_count_altitude, twi =summary_df$pred_count_twi))
  }
  return(res)
  }


summarise_max_ht_predictions <- function(model, quantiles = c(0.025, 0.975)) {
    samples <- rstan::extract(model$fit, 
                              pars = c('pred_ht_severity', 'pred_ht_altitude',
                                       'pred_ht_twi'))
    
    pred_var <- model$stan_data[c('sim_severity','sim_altitude','sim_twi')]
  
  lookup <- list(pred_ht_severity ='sim_severity',
                 pred_ht_altitude ='sim_altitude',
                 pred_ht_twi ='sim_twi')
  
  
  mapply(function(x, name) {
    if(length(dim(x))==2) {
      df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      cbind(pred_var[lookup[[name]]], df) 
    } else {
      cbind.data.frame(
        mean = mean(x),
        sd = sd(x),
        t(quantile(x,quantiles)))
    }
  }, x =samples, name =names(samples), SIMPLIFY = FALSE)
}
