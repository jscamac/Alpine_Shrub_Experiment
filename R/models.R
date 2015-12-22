# Model to fill censored poa distance data
run_censored_model <- function(data) {
  stan_data <- list(
    N = nrow(data),
    n_inds = length(unique(data$ind)),
    ind = as.numeric(factor(data$ind)),
    n_census = length(unique(data$date)),
    census = as.numeric(as.factor(data$date)),
    n_plots = length(unique(data$plot)),
    plot = data$plot,
    otc = data$otc,
    n_obs = nrow(data[data$is_censored==0,]),
    is_censored = data$is_censored,
    obs_index = which(data$is_censored== 0),
    n_censored = nrow(data[data$is_censored==1,]),
    censored_index = which(data$is_censored==1),
    y_obs = data$poa_distance_cm[data$is_censored==0])
  
  model <-'
    data {
      # Required for estimating distances
      int<lower=1> N; 
      int<lower = 1> n_obs; 
      int<lower = 1> n_censored;
      int<lower = 1> n_census;
      int<lower = 1> n_inds;
      int<lower = 1> n_plots;
      int<lower = 1> ind[N]; 
      int<lower = 1> plot[N];
      int<lower = 0, upper=1> otc[N];
      int<lower = 0> census[N];
      real<lower=0> y_obs[n_obs];
      # For merging back into one object
      int<lower=1, upper = N> censored_index[n_censored];
      int<lower=1, upper = N>  obs_index[n_obs];
    }
  
  parameters{
    #Estimating distances model
    real log_alpha;
    real log_b_otc;
    real raw_census[n_census];
    real raw_plot[n_plots];
    real raw_ind[n_inds];
    real<lower=0> sigma_log_ind;
    real<lower=0> sigma_log_plot;
    real<lower=0> sigma_log_census;
    real<lower=0> sigma_log_obs;
    real<lower=26, upper=100> y_censored[n_censored]; #Censored data lower limit = 26cm
  }
  
  transformed parameters{
    real log_census_error[n_census];
    real log_plot_error[n_plots];
    real log_ind_error[n_inds];
    real log_y_obs_hat[n_obs];
    real log_y_censored_hat[n_censored];
    real full_data[N];
    
    # Random effect for census
    for (t in 1:n_census) {
      log_census_error[t] <- raw_census[t] * sigma_log_census;
    }
    
    # Random effect for plot
    for (p in 1:n_plots) {
      log_plot_error[p] <- raw_plot[p] * sigma_log_plot;
    }
    
    # Random effect for individual
    for (i in 1:n_inds) {
      log_ind_error[i] <- raw_ind[i] * sigma_log_ind;
    }
    
    # Likelihood for observed
    for (i in 1:n_obs){
      log_y_obs_hat[i] <- log_alpha + log_b_otc * otc[i] + log_census_error[census[i]] + log_plot_error[plot[i]] + log_ind_error[ind[i]];
      
      # Adds observations to complete object
      full_data[obs_index[i]] <- y_obs[obs_index[i]];
    }
    # Estimates censored observations
    for (j in 1:n_censored){ 
      log_y_censored_hat[j] <- log_alpha + log_b_otc * otc[j] + log_census_error[census[j]] + log_plot_error[plot[j]] + log_ind_error[ind[j]];
      
      # Adds censored observations to complete object
      full_data[censored_index[j]] <- y_censored[j];
    }
  }
  model{
    # PRIORS
    log_alpha ~ normal(0,2.5);
    log_b_otc ~ normal(0, 2.5);
    raw_census ~ normal(0,1);
    raw_plot ~ normal(0,1);
    raw_ind ~ normal(0,1);
    sigma_log_census ~ cauchy(0, 25);
    sigma_log_plot ~ cauchy(0, 25);
    sigma_log_ind ~ cauchy(0, 25);
    sigma_log_obs ~ cauchy(0, 25);
    
    y_obs ~ lognormal(log_y_obs_hat, sigma_log_obs);
    y_censored ~ lognormal(log_y_censored_hat, sigma_log_obs);
  }'
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <-list(stan_data = stan_data, 
             fit = stan(model_code = model, 
                        data = stan_data,
                        pars = c("full_data", "sigma_log_census","sigma_log_plot","sigma_log_ind","sigma_log_obs","log_alpha","y_censored","log_b_otc","log_census_error","log_plot_error","log_ind_error"), 
                        chains = 3,
                        control=list(adapt_delta=0.995,stepsize=0.005, max_treedepth =15),
                        iter = 2000))
  return(fit)
}

## Gap dynamic model

run_gap_dynamic_model <- function(data, pred_n_years) {
  data <- filter(data, years!=0) %>% droplevels()
  
  pred_yr <- seq(0,pred_n_years, length.out = 100)
  stan_data <- list(
    N = nrow(data),
    n_inds = max(unique(data$id)),
    ind = data$id,
    n_plots = length(unique(data$plot)),
    plot = data$plot,
    otc = data$otc,
    year = data$year,
    initial_dist = data$initial_median_poadist,
    y_obs = data$median_poadist,
    pred_yr = pred_yr,
    n_preds = length(pred_yr))
  
  model <- '
    data {
      # Required for estimating distances
      int<lower=1> N; 
      int<lower = 1> n_inds;
      int<lower = 1> n_plots;
      int<lower = 1> ind[N]; 
      int<lower = 1> plot[N];
      int<lower=0, upper=1> otc[N];
      real year[N];
      real<lower=0> initial_dist[N];
      real<lower=0> y_obs[N];
      int<lower = 1> n_preds;
      real<lower=0> pred_yr[n_preds];
    }
  
  parameters{
    #Estimating distances model
    real alpha;
    real b_otc;
    real raw_plot[n_plots];
    real raw_ind[n_inds];
    real<lower=0> sigma_ind;
    real<lower=0> sigma_plot;
    real<lower=0> sigma_log_obs;
  }
  
  transformed parameters{
    real plot_error[n_plots];
    real ind_error[n_inds];
    real beta[N];
    real log_y_obs_hat[N];
    
    # Random effect for plot
    for (p in 1:n_plots) {
      plot_error[p] <- raw_plot[p] * sigma_plot;
    }
    
    # Random effect for individual
    for (i in 1:n_inds) {
      ind_error[i] <- raw_ind[i] * sigma_ind;
    }
    
    # Likelihood for observed
    for (i in 1:N){
      beta[i] <- alpha + b_otc * otc[i] + plot_error[plot[i]] + ind_error[ind[i]];
      log_y_obs_hat[i] <- log(initial_dist[i]) + beta[i] * year[i]; 
      # note above is on log scale due to lognormal everything is on log scale. on normal scale model is init * exp(beta * yr)
    }
  }
  model{
    
    # PRIORS
    alpha ~ normal(0, 5);
    b_otc ~ normal(0, 5);
    raw_plot ~ normal(0,1);
    raw_ind ~ normal(0,1);
    sigma_plot ~ cauchy(0, 25);
    sigma_ind ~ cauchy(0, 25);
    sigma_log_obs ~ cauchy(0, 25);
    
    y_obs ~ lognormal(log_y_obs_hat, sigma_log_obs);
  }
  generated quantities{
    real preds_otc[n_preds];
    real preds_ctl[n_preds];
    
    for (i in 1:n_preds) {
      preds_otc[i] <- exp(log(10) + ((alpha + b_otc)  * pred_yr[i]) + (sigma_log_obs^2)/2); # convert to arthamatic
      preds_ctl[i] <- exp(log(10) + (alpha  * pred_yr[i]) + (sigma_log_obs^2)/2);
    }
  }'
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data= stan_data,
                         pars = c('alpha','b_otc','sigma_plot','sigma_ind','sigma_log_obs', 'preds_otc','preds_ctl'), 
                         chains = 3,
                         iter = 2000, 
                         control=list(adapt_delta=0.99,stepsize=0.005, max_treedepth =15)))
  return(fit)
}

## non-tussock growth analysis
run_non_tussock_growth_analysis <- function(data, pred_n_years) {
  pred_year <- seq(0,pred_n_years,length.out=100)
  stan_data <- 
    list(
      n_obs = nrow(data),
      n_spp = length(unique(data$spp)),
      n_plots = length(unique(data$plot)),
      n_inds = length(unique(data$ind)),
      spp = as.numeric(factor(data$spp)),
      plot = data$plot,
      ind = as.numeric(factor(data$ind)),
      otc = data$otc,
      year = data$year,
      hmax = c(34,67,66,70),
      initial_height = data$initial_ht,
      height = data$ht,
      pred_year = pred_year,
      n_preds = length(pred_year))
  
  model="
    data {
      int<lower=1> n_obs;
      int<lower=1> n_spp;
      int<lower=1> n_plots;
      int<lower=1> n_inds;
      int<lower=1> spp[n_obs];
      int<lower=1> plot[n_obs];
      int<lower=1> ind[n_obs];
      real year[n_obs];
      int<lower=0, upper=1> otc[n_obs];
      real<lower=0> initial_height[n_obs];
      real<lower=0> height[n_obs];
      real<lower=0> hmax[n_spp];
      int<lower=1> n_preds;
      real<lower=0> pred_year[n_preds];
    }
  parameters { # Declare parameters the models must estimate
    real alpha[n_spp];
    real mu_alpha;
    real<lower=0> sigma_alpha;
    real b_otc[n_spp];
    real mu_b_otc;
    real<lower=0> sigma_b_otc;
    real<lower=0> sigma_plot;
    real<lower=0> sigma_ind;
    real<lower=0> sigma_obs;
    real ranef_plot[n_plots];
    real ranef_ind[n_inds];
  }
  model { # Define priors and likelihood
    real R[n_obs];
    real height_hat[n_obs];
    
    #Species random effects
    alpha ~ normal(mu_alpha, sigma_alpha);
    b_otc ~ normal(mu_b_otc, sigma_b_otc);
    
    #Plot random effects
    ranef_plot ~ normal(0, sigma_plot);
    
    # Estimate individual random effect
    ranef_ind ~ normal(0, sigma_ind);
    
    for (i in 1:n_obs) {
      # Estimate individual rate  
      R[i] <- 
        alpha[spp[i]] +     
        b_otc[spp[i]] * otc[i] +
        ranef_plot[plot[i]] +
        ranef_ind[ind[i]];
      
      #Likelihood
      height_hat[i] <- hmax[spp[i]] / (1 + (hmax[spp[i]]/initial_height[i] - 1) * exp(-R[i] * (year[i])));
      height[i] ~ normal(height_hat[i], sigma_obs)T[0,];
    }
    
    #Priors
    mu_alpha ~ normal(0,0.5);
    mu_b_otc ~ normal(0,0.5);
    sigma_alpha ~ cauchy(0,25);
    sigma_b_otc ~ cauchy(0,25);
    sigma_plot ~ cauchy(0,25);
    sigma_ind ~ cauchy(0,25);
    sigma_obs ~ cauchy(0,25);
  }
  generated quantities {
    real r_ctl[n_spp];
    real r_otc[n_spp];
    matrix[n_preds,n_spp] pred_height_ctl;
    matrix[n_preds,n_spp] pred_height_otc;
    
    for(s in 1:n_spp) {
      r_ctl[s] <- alpha[s];
      r_otc[s]  <- alpha[s] + b_otc[s];
      
      for(i in 1:n_preds){
        pred_height_ctl[i,s] <- hmax[s] / (1 + (hmax[s]/6 - 1) * exp(-r_ctl[s] * pred_year[i]));
        pred_height_otc[i,s] <- hmax[s] / (1 + (hmax[s]/6 - 1) * exp(-r_otc[s] * pred_year[i]));
      }
    }
  }"
  
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data= stan_data,
                         pars = c("mu_alpha","mu_b_otc",
                                  "sigma_alpha","sigma_b_otc","sigma_obs","sigma_ind",
                                  "alpha","b_otc","sigma_plot", "r_ctl","r_otc","pred_height_ctl","pred_height_otc"), 
                         chains = 3,
                         control=list(stepsize=0.001),
                         iter = 2000))
              return(fit)
}

## tussock growth analysis
run_tussock_growth_analysis <- function(data, pred_poadist_range = NULL) {
  if(is.null(pred_poadist_range)) {
  pred_poadist_range <- round(range(data$median_poadist),1)
  }
  cs_ln_median_poadist <- scale(log(data$median_poadist))
  sim_median_poadist <- seq(pred_poadist_range[1],pred_poadist_range[2],length.out = 100)
  cs_ln_sim_median_poadist <- scale(log(sim_median_poadist), attr(cs_ln_median_poadist, 'scaled:center'), 
                                    2 *attr(cs_ln_median_poadist, 'scaled:scale'))[,1]
  
  stan_data <- 
    list(
      n_obs = nrow(data),
      n_plots = length(unique(data$plot)),
      n_inds = length(unique(data$ind)),
      plot = data$plot,
      ind = as.numeric(factor(data$ind)),
      otc = data$otc,
      cs_ln_median_poadist = cs_ln_median_poadist[,1]/2,
      year = data$year,
      initial_height = data$initial_ht,
      height = data$ht,
      hmax = 67,
      cs_ln_sim_median_poadist = cs_ln_sim_median_poadist,
      n_preds = length(cs_ln_sim_median_poadist),
      sim_median_poadist = sim_median_poadist)
  
  model="
    data{
      int<lower=1> n_obs;
      int<lower=1> n_plots;
      int<lower=1> n_inds;
      int<lower=1> plot[n_obs];
      int<lower=1> ind[n_obs];
      real year[n_obs];
      int<lower=0, upper=1> otc[n_obs];
      real cs_ln_median_poadist[n_obs];
      real<lower=0> initial_height[n_obs];
      real<lower=0> height[n_obs];
      real<lower=0> hmax;
      int<lower=1> n_preds;
      real cs_ln_sim_median_poadist[n_preds];
    }
  parameters{ # Declare parameters the models must estimate
    real alpha;
    real b_otc;
    real b_poadist;
    real b_otc_x_poadist;
    real<lower=0> sigma_plot;
    real<lower=0> sigma_ind;
    real<lower=0> sigma_obs;
    real ranef_plot[n_plots];
    real ranef_ind[n_inds];
  }
  model { # Define priors and likelihood
    real R[n_obs];
    real height_hat[n_obs];
    
    #Plot random effects
    ranef_plot ~ normal(0, sigma_plot);
    
    # Estimate individual random effect
    ranef_ind ~ normal(0, sigma_ind);
    
    for (i in 1:n_obs) {
      # Estimate individual rate  
      R[i] <- 
        alpha +     
        b_otc * otc[i] +
        b_poadist * cs_ln_median_poadist[i] +
        b_otc_x_poadist * otc[i] * cs_ln_median_poadist[i] +
        ranef_plot[plot[i]] +
        ranef_ind[ind[i]];
      
      #Likelihood
      height_hat[i] <- hmax / (1 + (hmax/initial_height[i] - 1) * exp(-R[i] * (year[i])));
    }
      height ~ normal(height_hat, sigma_obs);
    
    #Priors
    alpha ~ normal(0, 0.5);
    b_otc ~ normal(0, 0.5);
    b_poadist ~ normal(0, 0.5);
    b_otc_x_poadist ~ normal(0, 0.5);
    sigma_plot ~ cauchy(0,25);
    sigma_ind ~ cauchy(0,25);
    sigma_obs ~ cauchy(0,25);
  }
  generated quantities{
    real r_ctl[n_preds];
    real r_otc[n_preds];
    
    for(i in 1:n_preds) {
      r_ctl[i] <- alpha + b_poadist * cs_ln_sim_median_poadist[i];
      r_otc[i]  <- alpha + b_otc + b_poadist * cs_ln_sim_median_poadist[i] + b_otc_x_poadist * cs_ln_sim_median_poadist[i];
    }
  }"
  
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data = stan_data,
                         pars = c("alpha","b_otc","b_poadist","b_otc_x_poadist",
                                  "sigma_plot","sigma_ind","sigma_obs",
                                  "r_ctl","r_otc"), 
                         chains = 3,
                         control=list(stepsize=0.001),
                         iter = 2000))
  return(fit)
}

run_non_tussock_mortalilty_model <- function(data) {
  stan_data <- 
    list(
      n_obs = nrow(data),
      n_spp = length(unique(data$spp)),
      n_plots = length(unique(data$plot)),
      n_inds = length(unique(data$ind)),
      spp = as.numeric(factor(data$spp)),
      plot = data$plot,
      ind = as.numeric(factor(data$ind)),
      otc = data$otc,
      census_interval = data$census_interval,
      died = data$dead_next_census)
  
  model="
    data {
      int<lower=1> n_obs;
      int<lower=1> n_spp;
      int<lower=1> n_plots;
      int<lower=1> n_inds;
      int<lower=1> spp[n_obs];
      int<lower=1> plot[n_obs];
      int<lower=1> ind[n_obs];
      real census_interval[n_obs];
      int<lower=0, upper=1> otc[n_obs];
      int<lower=0> died[n_obs];
    }
  parameters { # Declare parameters the models must estimate
    real raw_alpha[n_spp];
    real raw_b_otc[n_spp];
    real raw_plot[n_plots];
    real raw_ind[n_inds];
    real mu_alpha;
    real mu_b_otc;
    real<lower=0> sigma_alpha;
    real<lower=0> sigma_b_otc;
    real<lower=0> sigma_plot;
    real<lower=0> sigma_ind;
  }
  transformed parameters {
    real alpha[n_spp];
    real b_otc[n_spp];
    real ranef_plot[n_plots];
    real ranef_ind[n_inds];
    
    for(s in 1:n_spp){
      alpha[s] <- raw_alpha[s] * sigma_alpha + mu_alpha; //equivalent to normal(mu_alpha, sigma_alpha) 
      b_otc[s] <- raw_b_otc[s] * sigma_b_otc + mu_b_otc; //equivalent to normal(mu_b_otc, sigma_b_otc) 
    }
    
    for(p in 1:n_plots) {
      ranef_plot[p] <- raw_plot[p] * sigma_plot; //equivalent to normal(0, sigma_plot)
    }
    
    for(i in 1:n_inds) {
      ranef_ind[i] <- raw_ind[i] * sigma_ind; //equivalent to normal(0, sigma_ind) 
    }
  }
  model { # Define priors and likelihood
    real hazard[n_obs];
    real cumalative_hazard[n_obs];
    
    for (i in 1:n_obs) {
      # Estimate individual rate  
      hazard[i] <- 
        alpha[spp[i]] +     
        b_otc[spp[i]] * otc[i] +
        ranef_plot[plot[i]] +
        ranef_ind[ind[i]];
      
      #Likelihood
      cumalative_hazard[i] <- inv_cloglog(hazard[i] * (census_interval[i]));
    }
    died ~ bernoulli(cumalative_hazard);
    
    #Priors
    raw_alpha ~ normal(0,1);
    raw_b_otc ~ normal(0,1);
    raw_plot ~ normal(0,1);
    raw_ind ~ normal(0,1);
    mu_alpha ~ cauchy(0,2.5);
    mu_b_otc ~ cauchy(0,2.5);
    sigma_alpha ~ cauchy(0,25);
    sigma_b_otc ~ cauchy(0,25);
    sigma_plot ~ cauchy(0,25);
    sigma_ind ~ cauchy(0,25);
  }
  generated quantities {
    real p_death_ctl[n_spp];
    real p_death_otc[n_spp];
    
    for(s in 1:n_spp) {
      p_death_ctl[s] <- inv_cloglog(alpha[s]);
      p_death_otc[s]  <- inv_cloglog(alpha[s] + b_otc[s]);
    }
  }"
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data= stan_data,
                         pars = c("mu_alpha","mu_b_otc",
                                  "sigma_alpha","sigma_b_otc","sigma_plot","sigma_ind",
                                  "alpha","b_otc", "p_death_otc","p_death_ctl"), 
                         control=list(adapt_delta=0.999, stepsize=0.0001, max_treedepth =15),
                         chains = 3,
                         iter = 2000))
  return(fit)
}



run_tussock_mortality_model <- function(data, pred_poadist_range = NULL) {
  if(is.null(pred_poadist_range)) {
  pred_poadist_range <- round(range(data$median_poadist),1)
  }
  cs_ln_median_poadist <- scale(log(data$median_poadist))
  sim_median_poadist <- seq(pred_poadist_range[1],pred_poadist_range[2],length.out = 100)
  cs_ln_sim_median_poadist <- scale(log(sim_median_poadist), attr(cs_ln_median_poadist, 'scaled:center'),2 *attr(cs_ln_median_poadist, 'scaled:scale'))[,1]
  
    stan_data <- 
    list(
      n_obs = nrow(data),
      n_plots = length(unique(data$plot)),
      n_inds = length(unique(data$ind)),
      plot = data$plot,
      ind = as.numeric(factor(data$ind)),
      otc = data$otc,
      cs_ln_median_poadist = cs_ln_median_poadist[,1]/2,
      census_interval = data$census_interval,
      died = data$dead_next_census,
      cs_ln_sim_median_poadist = cs_ln_sim_median_poadist,
      n_preds = length(cs_ln_sim_median_poadist),
      sim_median_poadist = sim_median_poadist)
  
  model="
  data{
  int<lower=1> n_obs;
  int<lower=1> n_plots;
  int<lower=1> n_inds;
  int<lower=1> plot[n_obs];
  int<lower=1> ind[n_obs];
  real census_interval[n_obs];
  int<lower=0, upper=1> otc[n_obs];
  real cs_ln_median_poadist[n_obs];
  int<lower=0, upper=1> died[n_obs];
  int<lower=1> n_preds;
  real cs_ln_sim_median_poadist[n_preds];
  }
  parameters{ # Declare parameters the models must estimate
  real alpha;
  real b_otc;
  real b_poadist;
  real b_otc_x_poadist;
  real raw_ind[n_inds];
  real raw_plot[n_plots];
  real<lower=0> sigma_plot;
  real<lower=0> sigma_ind;
  }

  transformed parameters {
    real ranef_plot[n_plots];
    real ranef_ind[n_inds];
    
    for(p in 1:n_plots) {
      ranef_plot[p] <- raw_plot[p] * sigma_plot; //equivalent to normal(0, sigma_plot)
    }
    
    for(i in 1:n_inds) {
      ranef_ind[i] <- raw_ind[i] * sigma_ind; //equivalent to normal(0, sigma_ind) 
    }
  }
  model { # Define priors and likelihood
    real hazard[n_obs];
    real cumalative_hazard[n_obs];
    
    for (i in 1:n_obs) {
      # Estimate individual rate  
      hazard[i] <- 
        alpha +     
        b_otc * otc[i] +
        b_poadist * cs_ln_median_poadist[i] +
        b_otc_x_poadist * otc[i] * cs_ln_median_poadist[i] +
        ranef_plot[plot[i]] +
        ranef_ind[ind[i]];
      
      #Likelihood
      cumalative_hazard[i] <- inv_cloglog(hazard[i] * (census_interval[i]));
    }
    died ~ bernoulli(cumalative_hazard);
    
    #Priors
    raw_plot ~ normal(0,1);
    raw_ind ~ normal(0,1);
    alpha ~ cauchy(0,2.5);
    b_otc ~ cauchy(0,2.5);
    b_poadist ~ cauchy(0,2.5);
    b_otc_x_poadist ~ cauchy(0,2.5);
    sigma_plot ~ cauchy(0,25);
    sigma_ind ~ cauchy(0,25);
  }
  generated quantities{
  real p_death_ctl[n_preds];
  real p_death_otc[n_preds];
  
  for(i in 1:n_preds) {
  p_death_ctl[i] <- inv_cloglog(alpha + b_poadist * cs_ln_sim_median_poadist[i]);
  p_death_otc[i]  <- inv_cloglog(alpha + b_otc + b_poadist * cs_ln_sim_median_poadist[i] + b_otc_x_poadist * cs_ln_sim_median_poadist[i]);
  }
  }"
  
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data= stan_data,
                         pars = c("alpha","b_otc","b_poadist","b_otc_x_poadist",
                                  "sigma_plot","sigma_ind",
                                  "p_death_ctl","p_death_otc"), 
                         chains = 3,
                         iter = 2000, 
                         control=list(adapt_delta=0.999,stepsize=0.001, max_treedepth =15)))
  return(fit)
}

run_density_model <- function(data, species = "Grevillea") {
 data <- filter(data, spp==species) %>% droplevels()
 cov_stats <- density_height_covariate_summary(data)
 sims <- simulate_covariate_range(data)
 #Stan doesn't like NA's so replace NA with zeros
 data$min_twig_diam[is.na(data$min_twig_diam)] <- 0

 cs_severity <- scale(unique(data[,c('site','min_twig_diam')])$min_twig_diam, cov_stats['mean','min_twig_diam'], 2*cov_stats['sd','min_twig_diam'])[,1]
 cs_altitude <- scale(unique(data[,c('site','altitude_m')])$altitude_m, cov_stats['mean','altitude_m'],2*cov_stats['sd','altitude_m'])[,1]
 cs_twi <- scale(unique(data[,c('site','topo_wetness_index')])$topo_wetness_index, cov_stats['mean','topo_wetness_index'], 2*cov_stats['sd','topo_wetness_index'])[,1]
 cs_adult_den <- scale(unique(data[,c('site','transect','greaus_tran_adultden')])$greaus_tran_adultden, cov_stats['mean','greaus_tran_adultden'], 2*cov_stats['sd','greaus_tran_adultden'])[,1]

 cs_sim_severity <- scale(sims$sim_severity, cov_stats['mean','min_twig_diam'], 2*cov_stats['sd','min_twig_diam'])[,1]
 cs_sim_altitude <- scale(sims$sim_altitude, cov_stats['mean','altitude_m'],2*cov_stats['sd','altitude_m'])[,1]
 cs_sim_twi <- scale(sims$sim_twi, cov_stats['mean','topo_wetness_index'], 2*cov_stats['sd','topo_wetness_index'])[,1]
 cs_sim_adult_den <- scale(sims$sim_adult_den, cov_stats['mean','greaus_tran_adultden'], 2*cov_stats['sd','greaus_tran_adultden'])[,1]
 
stan_data <- 
    list(
      n_obs = nrow(data),
      n_sites = length(unique(data$site)),
      n_transects = nrow(unique(data[,c('site','transect')])),
      site = as.integer(data$site),
      transect = as.integer(as.factor(paste(data$site,data$transect, sep='_'))),
      burnt = unique(data[,c('site','burnt_03')])$burnt_03,
      cs_severity = cs_severity,
      cs_altitude = cs_altitude,
      cs_twi = cs_twi,
      cs_adult_den = cs_adult_den,
      n_sims = length(cs_sim_adult_den),
      cs_sim_severity = cs_sim_severity,
      cs_sim_altitude = cs_sim_altitude,
      cs_sim_twi = cs_sim_twi,
      cs_sim_adult_den = cs_sim_adult_den,
      y = data$seedling_density,
      sim_severity = sims$sim_severity,
      sim_altitude = sims$sim_altitude,
      sim_twi = sims$sim_twi,
      sim_adult_den = sims$sim_adult_den)



model=sprintf("
  data{ # Declare what each data variable is
    int<lower=1> n_obs;
    int<lower=1> n_sites;
    int<lower=1> n_transects;
    int<lower=1> site[n_obs];
    int<lower=1> transect[n_obs];
    int<lower=0> burnt[n_sites];
    real cs_severity[n_sites];
    real cs_altitude[n_sites];
    real cs_twi[n_sites];
    %s
    int<lower=0> y[n_obs]; 
    int<lower=0> n_sims;
    real cs_sim_severity[n_sims];
    real cs_sim_altitude[n_sims];
    real cs_sim_twi[n_sims];
    %s
  }
parameters{ # Declare parameters the models must estimate

  real alpha_mu;
  real<lower=0> alpha_sigma;
  real raw_alpha[n_sites];
  real b_unburnt;
  real b_severity;
  real b_altitude;
  real b_twi;
  real b_transect_mu;
  real<lower=0> b_transect_sigma;
  real b_raw_transect[n_transects];
  %s
  real<lower=0, upper=100> phi;
}
transformed parameters { # Declare and define derived variables used in model
  real count[n_obs];
  real alpha[n_sites];
  real a_transect[n_transects];
  real b_transect[n_transects];
  

     for (t in 1:n_transects) {
     b_transect[t] <- b_raw_transect[t] * b_transect_sigma;
     }

  for (s in 1:n_sites) {
  alpha[s] <- raw_alpha[s] * alpha_sigma + alpha_mu;
  }
  
  for (i in 1:n_obs) {
    count[i] <- exp(alpha[site[i]] + 
                    b_unburnt * (1-burnt[site[i]]) + 
                    b_severity * (cs_severity[site[i]] * burnt[site[i]]) +
                    b_altitude * cs_altitude[site[i]] + 
                    b_twi * cs_twi[site[i]] + 
                    %s
                    b_transect[transect[i]]);
  }
}
model { # Define priors and likelihood
  
  # PRIORS
  raw_alpha ~ normal(0,1);
  alpha_mu ~ normal(0, 2.5);
  alpha_sigma ~ cauchy(0, 25);
  b_unburnt ~ normal(0,2.5);
  b_severity ~ normal(0,2.5);
  b_altitude ~ normal(0,2.5);
  b_twi ~ normal(0,2.5);
  b_raw_transect ~ normal(0,1);
  b_transect_sigma ~ cauchy(0,25);
  %s

  
  # Likelihood
    y ~ neg_binomial_2(count, phi);
}
generated quantities { # Calculate log likelihood, residuals or make predictions
  
  # Parameters to calculate log likelihood
  
  # Predictions
  real pred_count_unburnt;
  real pred_count_burnt;
  real pred_count_severity[n_sims];
  real pred_count_altitude[n_sims];
  real pred_count_twi[n_sims];
  %s

  # Partial dependencies

  pred_count_unburnt <- exp(alpha_mu + b_unburnt);
  pred_count_burnt<- exp(alpha_mu);

  for(i in 1:n_sims) {
  pred_count_severity[i] <- exp(alpha_mu + b_severity * cs_sim_severity[i]);
  pred_count_altitude[i] <- exp(alpha_mu + b_altitude * cs_sim_altitude[i]);
  pred_count_twi[i] <- exp(alpha_mu + b_twi * cs_sim_twi[i]);
  %s
  }
}",ifelse(species =='Grevillea', "real cs_adult_den[n_transects];", ""),
   ifelse(species =='Grevillea', "real cs_sim_adult_den[n_sims];",""),
   ifelse(species =='Grevillea', "real b_adult_density;",""),
   ifelse(species =='Grevillea', "b_adult_density * cs_adult_den[transect[i]] +",""),
   ifelse(species =='Grevillea', "b_adult_density ~ normal(0, 2.5);",""),
   ifelse(species =='Grevillea', "real pred_count_adult_density[n_sims];",""),
   ifelse(species =='Grevillea', "pred_count_adult_density[i] <- exp(alpha_mu + b_adult_density * cs_sim_adult_den[i]);",""))


  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  if(species =="Grevillea") {
    pars <- c("alpha_mu", "alpha_sigma","b_transect_sigma",
              "b_unburnt","b_severity","b_altitude","b_twi","phi", "b_adult_density",
              "pred_count_unburnt","pred_count_burnt","pred_count_severity","pred_count_altitude",
              "pred_count_twi","pred_count_adult_density")
  }else {
    pars <- c("alpha_mu", "alpha_sigma","b_transect_sigma",
              "b_unburnt","b_severity","b_altitude","b_twi","phi",
              "pred_count_unburnt","pred_count_burnt","pred_count_severity","pred_count_altitude",
              "pred_count_twi")}
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data= stan_data,
                         pars = pars,
                         chains = 3,
                         iter = 2000, 
                         control=list(adapt_delta =0.9,stepsize=0.05, max_treedepth =15)))
  return(fit)
}


run_max_height_model <- function(data, species) {
 data <- filter(data, !is.na(max_seedling_height_cm) & spp==species & burnt_03==1) %>% droplevels()
 cov_stats <- density_height_covariate_summary(data)
 sims <- simulate_covariate_range(data)

 cs_severity <- scale(unique(data[,c('site','min_twig_diam')])$min_twig_diam, cov_stats['mean','min_twig_diam'], 2*cov_stats['sd','min_twig_diam'])[,1]
 cs_altitude <- scale(unique(data[,c('site','altitude_m')])$altitude_m, cov_stats['mean','altitude_m'],2*cov_stats['sd','altitude_m'])[,1]
 cs_twi <- scale(unique(data[,c('site','topo_wetness_index')])$topo_wetness_index, cov_stats['mean','topo_wetness_index'], 2*cov_stats['sd','topo_wetness_index'])[,1]

 cs_sim_severity <- scale(sims$sim_severity, cov_stats['mean','min_twig_diam'], 2*cov_stats['sd','min_twig_diam'])[,1]
 cs_sim_altitude <- scale(sims$sim_altitude, cov_stats['mean','altitude_m'],2*cov_stats['sd','altitude_m'])[,1]
 cs_sim_twi <- scale(sims$sim_twi, cov_stats['mean','topo_wetness_index'], 2*cov_stats['sd','topo_wetness_index'])[,1]
 
stan_data <- 
    list(
      n_obs = nrow(data),
      n_sites = length(unique(data$site)),
      site = as.integer(as.factor(as.character(data$site))),
      cs_severity = cs_severity,
      cs_altitude = cs_altitude,
      cs_twi = cs_twi,
      n_sims = length(cs_sim_twi),
      cs_sim_severity = cs_sim_severity,
      cs_sim_altitude = cs_sim_altitude,
      cs_sim_twi = cs_sim_twi,
      sim_severity = sims$sim_severity,
      sim_altitude = sims$sim_altitude,
      sim_twi = sims$sim_twi,
      y = data$max_seedling_height_cm)

  model <- '
  data{ # Declare what each data variable is
    int<lower=1> n_obs;
    int<lower=1> n_sites;
    int<lower=1> site[n_obs];
    real cs_severity[n_sites];
    real cs_altitude[n_sites];
    real cs_twi[n_sites];
    real<lower=0> y[n_obs]; 
    int<lower=0> n_sims;
    real cs_sim_severity[n_sims];
    real cs_sim_altitude[n_sims];
    real cs_sim_twi[n_sims];
  }
  parameters {
  real raw_site[n_sites];
  real mu_log_site;
  real<lower=0> sigma_log_site;
  real log_b_severity;
  real log_b_altitude;
  real log_b_twi;
  real<lower=0> sigma_log_obs;
  }
  transformed parameters {
  real log_alpha_site[n_sites];
  for (s in 1:n_sites) {
  log_alpha_site[s] <- raw_site[s] * sigma_log_site + mu_log_site;
  }
  }
  model {
  real log_max_ht_hat[n_obs];

  for (i in 1:n_obs) {
  log_max_ht_hat[i] <- log_alpha_site[site[i]] +
                   log_b_severity * cs_severity[site[i]] +
                   log_b_altitude * cs_altitude[site[i]] +
                   log_b_twi * cs_twi[site[i]];
  }
  y ~ lognormal(log_max_ht_hat, sigma_log_obs);
  
  raw_site ~ normal(0,1);
  log_b_severity ~ normal(0,2.5);
  log_b_altitude ~ normal(0,2.5);
  log_b_twi ~ normal(0,2.5);
  mu_log_site ~ normal(0,2.5);
  sigma_log_site ~ cauchy(0,25);
  sigma_log_obs ~ cauchy(0, 25);
}
generated quantities { # Calculate log likelihood, residuals or make predictions
  
  # Parameters to calculate log likelihood
  
  # Predictions
  real pred_ht_severity[n_sims];
  real pred_ht_altitude[n_sims];
  real pred_ht_twi[n_sims];

  # Partial dependencies

  for(i in 1:n_sims) {
  pred_ht_severity[i] <- exp((mu_log_site + log_b_severity * cs_sim_severity[i]) + (sigma_log_obs^2)/2);
  pred_ht_altitude[i] <- exp((mu_log_site + log_b_altitude * cs_sim_altitude[i]) + (sigma_log_obs^2)/2);
  pred_ht_twi[i] <- exp((mu_log_site + log_b_twi * cs_sim_twi[i]) + (sigma_log_obs^2)/2);
  }
}'
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit <- list(stan_data = stan_data,
            fit =stan(model_code = model, 
                      data= stan_data,
                      pars = c('mu_log_site','log_b_severity','log_b_altitude','log_b_twi',
                               'sigma_log_site','sigma_log_obs','pred_ht_severity','pred_ht_altitude','pred_ht_twi'),
                      chains = 3,
                      iter = 2000, 
                      control=list(adapt_delta =0.9,stepsize=0.05, max_treedepth =15)))
            return(fit)
}

