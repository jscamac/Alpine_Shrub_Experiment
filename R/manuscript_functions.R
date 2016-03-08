# These functions are for extracting key values for use in manuscript

# Extract mean density estimates
density_means <- function(model,species, burnt=TRUE) {
  if(species == 'Grevillea') {
  density <- round(summarise_density_predictions(model, 'Grevillea')$burnt_unburnt$mean,2)
  } else {
  density <- round(summarise_density_predictions(model, 'Asterolasia')$burnt_unburnt$mean,2)
  }
  if(isTRUE(burnt)) {
    return(density[2])
  } else
    return(density[1])
}

# Calculate density ratios
density_ratios <- function(model, species) {
  if(species == 'Grevillea') {
  density <- round(summarise_density_predictions(model, 'Grevillea')$burnt_unburnt$mean,2)
  } else {
  density <- round(summarise_density_predictions(model, 'Asterolasia')$burnt_unburnt$mean,2)
  }
  round(density[2]/density[1])
}

# Extract climate statistics
micro_clim <- function(data, clim_var, summary_var, statistic) {
  round(subset(data, sensor==clim_var & clim_stat == summary_var)[[statistic]],1)
}

# Extract height differences 
ht_diff <- function(data,species) {
  max_date <- max(data$date)
  ctl_ht <- subset(summarise_otc_growth_obs(data, 'ht'),date== max_date & otc=='ctl' & spp == species)
  otc_ht <- subset(summarise_otc_growth_obs(data, 'ht'),date== max_date & otc=='otc' & spp == species)
  round(otc_ht$mean - ctl_ht$mean,1)
}

# Calculate growth rate ratios between otc and ctl
growth_rate_ratio <- function(model,species) {
  mod <- summarise_coefficients(model,c('alpha','b_otc'))
  if(species =='Asterolasia') {
    n = 1
  }
  if(species =='Grevillea') {
    n = 2
  }
  if(species =='Phebalium') {
    n = 3
  }
  if(species =='Prostanthera') {
    n = 4
  }
  pars <- c(paste0('alpha_',n), paste0('b_otc_',n))
  res <- subset(mod, parameter %in% pars)
  round((res$mean[1] + res$mean[2])/res$mean[1],1)
}

# Extract year max height is predicted to be reached
hmax <- function(model, species, diff=TRUE) {
  predictions <- summarise_otc_model_predictions('non_tussock_growth',model)
  res <- extract_hmax_year(predictions)
  res <- subset(res, spp == species)
  if(diff==TRUE) {
  round(subset(res, treatment=='ctl')$pred_year - subset(res, treatment=='otc')$pred_year)
    } else {
      round(res$pred_year)
    }
}

# Calculate validation max height at 1660 and 1860
max_ht_alt <- function(model,diff = TRUE) {
  maxht <- summarise_max_ht_predictions(model)$pred_ht_altitude
  if(diff == TRUE) {
  round(maxht$mean[maxht$sim_altitude==1670] - maxht$mean[maxht$sim_altitude==1860])
  } else {
  round(subset(maxht, sim_altitude %in% c(1670,1860))$mean)
  }
}

# Calculate total deaths by number or percentage
total_deaths <- function(data,percentage=TRUE) {
  n_inds <- length(levels(data$ind))
  dat <- data %>%
    summarise(deaths = sum(dead_next_census))
  if(isTRUE(percentage)) {
    res <- round(dat$deaths/n_inds*100)
  } else {
    res <- dat$deaths
  }
  return(res)
}

# Count number of deaths by species
death_counts <- function(data,species) {
  dat <- data %>%
    group_by(spp) %>%
    summarise(deaths = sum(dead_next_census)) %>%
    filter(spp==species)
  return(dat$deaths)
}

# Extract annual mortality by species/treatment 
annual_mort <- function(model, species, trt) {
  mortality <- summarise_otc_model_predictions('non_tussock_mortality',model)
  round(subset(mortality, spp == species & treatment == trt)$mean*100)
}

# Calculate number of days experiment was conducted in total and chamber only
n_days <- function(data,chamber_only=TRUE) {
  min_date <- min(data$chambers_in)
  max_date <- max(data$chambers_in, na.rm=TRUE)
  dat <- vector()
  if(chamber_only==TRUE) {  
    for(i in 1:nrow(data)) {
      dat[i] <- as.vector((data$chambers_in[i+1] - data$chambers_out[i])) 
    }
    dat <- sum(dat, na.rm=TRUE)
  } else {
    dat <- as.numeric(max_date - min_date)
  }
  return(dat)
}

# hmax prior data
hmax_values <- function(data, spp) {
  return(subset(data, species == spp)$mn_max_ht)
}

# Extract mean change in gap size after 10 years
gap_size <- function(model, trt) {
  gap <- summarise_otc_model_predictions('gap_dynamics',model)
  gap <- subset(gap, treatment==trt)
  diff <- round(subset(gap, pred_yr==0)$mean - subset(gap, pred_yr==max(gap$pred_yr))$mean)
  return(diff)
}

# Average initial gap size
mean_init_gap <- function(data) {
  round(mean(data$median_poadist))
}

# range initial gap size
range_init_gap <- function(data) {
  round(range(data$median_poadist),1)
}
# Range of initial seedling sizes
range_init_size <- function(data) {
  dat <- subset(data,community=='open_heath' & census==0)
  round(range(dat$ht))
}

# Proportion censored distances
prop_censored <- function(data) {
  round(nrow(subset(data,is_censored==1))/nrow(subset(data,is_censored==0))*100)
}

# Extract Overdispersion
phi_value <- function(model, stat ='mean') {
res <- rstan::summary(model,'phi')$summary[,c('mean',"2.5%","97.5%")]
names(res) <- c('mean','l_bci','u_bci')
return(round(res[[stat]],2))
}


