
# Theme for partial plots
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(color = "black", face = "bold", size = 10) else element_blank()
  theme_classic() + theme(axis.title = element_text(face = "bold", size = 10),
                          axis.text = element_text(size = 10),
                          strip.text = st,
                          legend.title = element_text(size = 10),
                          legend.text = element_text(size = 10), 
                          plot.title = element_text(size = 18),
                          legend.title = element_blank(),
                          strip.background = sb,
                          legend.position = legend.position,
                          panel.margin = unit(4,"mm"))
}

# Theme for coefficient plots
coefficent_plot_theme <- function() {
  theme_classic() + theme(axis.title.x = element_text(face = "bold", size = 10),
                          axis.title.y = element_blank(),
                          axis.text = element_text(size = 10),
                          plot.title = element_text(size = 18),
                          title = element_text(face = "bold"),
                          panel.margin = unit(4,"mm"))
}


# Plot observed height growth
plot_obs_growth <- function(observed_data, scales ='free', ncol=4, 
                            ylab = 'Observed height (cm)', 
                            xlab ='Observation year', 
                            ylim = c(0,50)) {
  
  summary_obs <- summarise_otc_ht_obs(observed_data)
  
  ggplot(observed_data, aes(x = date,y = ht, colour=as.factor(otc))) + 
    geom_path(alpha = 0.4, aes(group = ind)) +
    geom_path(data = summary_obs, aes(x=date,y=ht),size=1.5,alpha =0.8) +
    geom_pointrange(data= summary_obs,aes(ymin = lower_95ci, ymax=upper_95ci),size=0.7) +
    scale_colour_manual('',values = c("0" ="sky blue","1" ="orange","ctl" ="blue","otc" ="red")) +
    partial_plot_theme(strips = TRUE) + 
    ylim(ylim) +
    ylab(ylab) + 
    xlab(xlab) + 
    facet_wrap(~spp,ncol=ncol, scale=scales) 
}

# Plot regression coefficients
coefficient_plot <- function(summarised_coefficients, y_axis_labels='',xlab ='Effect size', title=NULL) {
  
  
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter), size=0.5)+
    geom_segment(aes(x=`10%`,y=parameter, xend=`90%`, yend=parameter), size=1)+
    geom_point(aes(x=mean, y=parameter), size =3) +
    geom_vline(aes(x=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    labs(title=title) +
    coefficent_plot_theme()
}


# Plot predicted height growth
plot_growth_curves <- function(non_tussock_otc_growth_model, scales ='free', ncol=4, ylab = 'Predicted height (cm)', xlab ='Year') {
  predictions <- summarise_otc_model_predictions('non_tussock_growth',non_tussock_otc_growth_model)
  vert_line <- extract_hmax_year(predictions)
  
  ggplot(predictions, aes(x = pred_year,y = mean, colour = as.factor(treatment), fill = as.factor(treatment)), axis.line = element_line()) + 
    geom_line() +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    scale_fill_manual("",labels = c("Control", "Open Top Chamber"),values= c('ctl' ='blue','otc'='red')) +
    scale_color_manual("",labels = c("Control", "Open Top Chamber"),values= c('ctl' ='blue','otc'='red')) +
    geom_segment(data=vert_line,aes(x=pred_year,y=hmax, xend=pred_year, yend=-Inf),linetype=2, na.rm=TRUE) +
    ylim(0,80) + 
    ylab(ylab) + 
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) + 
    partial_plot_theme(strips = TRUE) +
    facet_wrap(~spp, ncol=ncol,scale=scales) 
}

# Predictive partial plots for tussock seedlings
poadist_plot <- function(summarised_predictions, xlab=NULL,ylab=NULL) {
  ggplot(summarised_predictions, aes(x = sim_median_poadist,y = mean, group=as.factor(treatment), colour = as.factor(treatment), fill = as.factor(treatment)), axis.line = element_line()) + 
    geom_line(size =1.2) +
    scale_colour_manual("",labels = c("Control", "Open Top Chamber"),values= c('ctl' ='blue','otc' ='red')) +
    scale_fill_manual("",labels = c("Control", "Open Top Chamber"),values= c('blue','red')) +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    ylab(ylab) + 
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) +
    partial_plot_theme(strips=FALSE)
}

# Predicted mortality in OTC vs CTL for non tussock seedlings
non_tussock_mortality_plot <- function(non_tussock_mortality_model, ylab ='Annual probability of death', xlab ='Species') {
  mortality <- summarise_otc_model_predictions('non_tussock_mortality',non_tussock_mortality_model)
  
  
  ggplot(mortality, aes(x = spp,y = mean, group= treatment, colour=treatment)) + 
    geom_point(position = position_dodge(.1), size=1) +
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`),size=1, position=position_dodge(.1)) +
    scale_colour_manual('',values = c("ctl" ="blue","otc" ="red")) +
    ylab(ylab) + 
    xlab(xlab) +
    partial_plot_theme()
}

# Plots covariate partial plots for density and height models
partial_plot_density_height <- function(predictions, x, xlab=NULL,ylab=NULL, ylim =c(0,15)) {
  ggplot(predictions, aes_string(x = x,y = 'mean'), axis.line = element_line()) + 
    geom_line(size =1.2) +
    scale_fill_manual("",values= 'grey60') +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    scale_x_continuous(expand=c(0,0)) +
    ylim(ylim) +
    xlab(xlab) +
    ylab(ylab) +
    partial_plot_theme(strips=FALSE)
}

burnt_v_unburnt <- function(predictions, x, ylab=NULL, ylim=c(0,1)) {
  ggplot(predictions, aes_string(x = x,y = 'mean')) + 
    geom_point(size=1) +
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`),size=1) +
    scale_x_discrete(labels =c('Unburnt','Burnt')) +
    xlab('') +
    ylab(ylab) +
    ylim(ylim) +
    partial_plot_theme()
}

gap_dynamics_curve <- function(summarised_predictions, xlab='Year',ylab='Median inter-tussock gap radius (cm)',ylim=c(0,20)) {
  ggplot(summarised_predictions, aes(x = pred_yr,y = mean, group=as.factor(treatment), colour = as.factor(treatment), fill = as.factor(treatment)), axis.line = element_line()) + 
    geom_line(size =1.2) +
    scale_colour_manual("",labels = c("Control", "Open Top Chamber"),values= c('ctl' ='blue','otc' ='red')) +
    scale_fill_manual("",labels = c("Control", "Open Top Chamber"),values= c('blue','red')) +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    ylab(ylab) + 
    ylim(ylim) +
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) +
    partial_plot_theme(strips=FALSE)
}

# PANEL PLOTS

# Observed and predicted growth curves
obs_pred_growth <- function(observed_data, non_tussock_otc_growth_model) {
  p1 <- plot_obs_growth(observed_data)
  p2 <- plot_growth_curves(non_tussock_otc_growth_model)
  grid.arrange(p1, p2, ncol=1)
}

# Partial plots for growth and mortality against poa distance size
tussock_plots <- function(tussock_growth_model, tussock_mortality_model, gap_dynamic_model) {
  growth <- summarise_coefficients(tussock_growth_model, c('alpha','b_otc','b_poadist','b_otc_x_poadist'))
  mortality <- summarise_coefficients(tussock_mortality_model, c('alpha','b_otc','b_poadist','b_otc_x_poadist'))
  growth_preds <- summarise_otc_model_predictions('tussock_growth',tussock_growth_model)
  mortality_preds <- summarise_otc_model_predictions('tussock_mortality',tussock_mortality_model)
  p1 <- coefficient_plot(growth, y_axis_labels = c('intercept (ctl)','otc','tussock gap','otc x tussock gap'), 
                         xlab = 'Growth coefficients')
  p2 <- poadist_plot(growth_preds,xlab = 'median inter-tussock gap size',ylab = 'R parameter')
  p3 <- coefficient_plot(mortality, y_axis_labels = c('intercept (ctl)','otc','tussock gap','otc x tussock gap'),
                         xlab = 'Mortality coefficents (cloglog scale)')
  
  p4 <- poadist_plot(mortality_preds, xlab = 'median inter-tussock gap size',ylab = 'Annual probability of death')
  grid.arrange(p1,p2,p3,p4, ncol=2)
}

density_count_plots <- function(density_model, species, ylim=c(0,20)) {
  if(species=='Grevillea') {
    params <- c('b_site_mu','b_unburnt','b_severity','b_altitude','b_twi','b_adult_density','phi')
    y_axis_labels <- c('Intercept','unburnt','severity','altitude','twi','adult density','phi')
  }
  if(species=='Asterolasia'){
    params <- c('b_site_mu','b_unburnt','b_severity','b_altitude','b_twi','phi')
    y_axis_labels <- c('Intercept','unburnt','severity','altitude','twi','phi')
  }
  coeffs <- summarise_coefficients(density_model,params = params)
  predictions <- summarise_density_predictions(density_model,species)
  p1 <- coefficient_plot(coeffs,y_axis_labels,xlab = 'log coefficients')
  p2 <- burnt_v_unburnt(predictions$count_model$burnt_unburnt,x ='predictor',ylab = expression(bold('Seedlings /'~m^2)))
  p3 <- partial_plot_density_height(predictions$count_model$severity, x ='sim_severity', 
                                    xlab ='Minimum Twig diameter (mm)', ylab =expression(bold('Seedlings /'~m^2)),ylim)
  p4 <- partial_plot_density_height(predictions$count_model$altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab =expression(bold('Seedlings /'~m^2)),ylim) 
  p5 <- partial_plot_density_height(predictions$count_model$twi, x ='sim_twi', 
                                    xlab ='Topographic wetness index', ylab =expression(bold('Seedlings /'~m^2)),ylim)
  
  if(species=='Grevillea') {
    p6 <- partial_plot_density_height(predictions$count_model$adult_density, x ='sim_adult_den', 
                                      xlab = expression(bold('Adult density /500'~m^2)), ylab = expression(bold('Seedlings /'~m^2)), ylim)
    grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
  }
  if(species=='Asterolasia') {
    grid.arrange(p1,p2,p3,p4,p5, ncol=2)
  }
}
density_absence_plots <- function(density_model, species, ylim=c(0,1)) {
  if(species=='Grevillea') {
    params <- c('a_site_mu','a_unburnt','a_severity','a_altitude','a_twi','a_adult_density')
    y_axis_labels <- c('Intercept','unburnt','severity','altitude','twi','adult density')
  }
  if(species=='Asterolasia'){
    params <-c('a_site_mu','a_unburnt','a_severity','a_altitude','a_twi')
    y_axis_labels <- c('Intercept','unburnt','severity','altitude','twi')
  }
  coeffs <- summarise_coefficients(density_model,params)
  predictions <- summarise_density_predictions(density_model,species)
  p1 <- coefficient_plot(coeffs,y_axis_labels,xlab = 'logit coefficients')
  p2 <- burnt_v_unburnt(predictions$absence_model$burnt_unburnt,
                        x = 'predictor', ylab = expression(bold('Pr(zero seedlings) /'~m^2)))
  p3 <- partial_plot_density_height(predictions$absence_model$severity, 
                                    x = 'sim_severity', xlab = 'Minimum Twig diameter (mm)', 
                                    ylab = expression(bold('Seedlings /'~m^2)),ylim) 
  p4 <- partial_plot_density_height(predictions$absence_model$altitude, 
                                    x = 'sim_altitude', xlab ='Altitude (m)', 
                                    ylab = expression(bold('Pr(zero seedlings) /'~m^2)),ylim)  
  p5 <- partial_plot_density_height(predictions$absence_model$twi, 
                                    x = 'sim_twi', xlab = 'Topographic wetness index', 
                                    ylab = expression(bold('Pr(zero seedlings) /'~m^2)),ylim) 
  if(species=='Grevillea') {
    p6 <- partial_plot_density_height(predictions$absence_model$adult_density, 
                                      x = 'sim_adult_den', xlab = expression(bold('Adult density /500'~m^2)), 
                                      ylab = expression(bold('Pr(zero seedlings) /'~m^2)), ylim) 
    grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
  }
  if(species=='Asterolasia') {
    grid.arrange(p1,p2,p3,p4,p5, ncol=2)
  }
}


max_ht_plots <- function(greaus_max_ht_model,asttry_max_ht_model, ylim=c(0,35)) {
  
  coeffs_greaus <- summarise_coefficients(greaus_max_ht_model,params = c('mu_log_site','log_b_severity','log_b_altitude','log_b_twi'))
  predictions_greaus <- summarise_max_ht_predictions(greaus_max_ht_model)
  
  coeffs_asttry <- summarise_coefficients(asttry_max_ht_model,params = c('mu_log_site','log_b_severity','log_b_altitude','log_b_twi'))
  predictions_asttry <- summarise_max_ht_predictions(asttry_max_ht_model)
  
  p1 <- coefficient_plot(coeffs_greaus,c('Intercept','severity','altitude','twi'),xlab = 'log coefficients', title='Grevillea')
  p2 <- coefficient_plot(coeffs_asttry,c('Intercept','severity','altitude','twi'),xlab = 'log coefficients', title='Asterolasia')
  p3 <- partial_plot_density_height(predictions_greaus$pred_ht_severity, x ='sim_severity', 
                                    xlab ='Minimum Twig diameter (mm)', ylab ='Maximum height (cm)',ylim)
  p4 <- partial_plot_density_height(predictions_asttry$pred_ht_severity, x ='sim_severity', 
                                    xlab ='Minimum Twig diameter (mm)', ylab ='Maximum height (cm)',ylim)
  p5 <- partial_plot_density_height(predictions_greaus$pred_ht_altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab ='Maximum height (cm)',ylim) 
  p6 <- partial_plot_density_height(predictions_asttry$pred_ht_altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab ='Maximum height (cm)',ylim) 
  p7 <- partial_plot_density_height(predictions_greaus$pred_ht_twi, x ='sim_twi', 
                                    xlab ='Topographic wetness index', ylab ='Maximum height (cm)',ylim)
  p8 <- partial_plot_density_height(predictions_asttry$pred_ht_twi, x ='sim_twi', 
                                    xlab ='Topographic wetness index', ylab ='Maximum height (cm)',ylim)
  
  grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2)
}

gap_dynamics_plot <- function(gap_dynamic_model) {
  coeffs <- summarise_coefficients(gap_dynamic_model, params = c('alpha','b_otc'))
  predictions <- summarise_otc_model_predictions('gap_dynamics',gap_dynamic_model)
  p1 <- coefficient_plot(coeffs,y_axis_labels = c('Intercept', 'otc'), xlab = 'log coefficients')
  p2 <- gap_dynamics_curve(predictions)
    grid.arrange(p1,p2, ncol=2)
}

examine_plot_microclimates <- function(hourly_microclimate, 
                                       subset_sensor = 'CAmbient_Temp', 
                                       stat ='mean', 
                                       subset_site = 'ITEX2.0',
                                       ylab = expression('Ambient temperature '(~degree~C)),
                                       xlim = NULL) {
  chamber_dates <- readRDS('raw_data/chamber_dates.rds')
  
  dat <- filter(hourly_microclimate, sensor == subset_sensor & site == subset_site)
  dat <- aggregate_microclimate_to_day(dat, aggregate_level = 'plot')
  if(is.null(xlim)) {
  min_date <- min(dat$date)
  max_date <- max(dat$date)
  xlim <- c(min_date,max_date)
  }
  
  ggplot(dat, aes_string(x = 'date',y = stat, colour='treatment'), axis.line = element_line()) +
    scale_colour_manual('',values = c("CTL" ="blue","OTC" ="red")) +
    geom_rect(data = chamber_dates, aes(xmin = chambers_in, xmax = chambers_out, 
                                        ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes=FALSE) + 
    geom_path() +
    ylab(ylab) +
    scale_x_date(expand=c(0,0), limits = xlim) +
    partial_plot_theme(strips = TRUE) +
    facet_wrap(~ plot, ncol=2, scales='fixed')
}

plot_microclim_trt_diff <- function(hourly_microclimate, 
                                    subset_site = 'ITEX2.0', 
                                    subset_sensor = 'CAmbient_Temp', 
                                    stat = 'mean', 
                                    ylab= NULL) {
  if(is.null(ylab)) {
    ylab <- subset_sensor
  }
  chamber_dates <- readRDS('raw_data/chamber_dates.rds')
  dat <- filter(hourly_microclimate, sensor == subset_sensor & site == subset_site)
  dat <- aggregate_microclimate_to_day(dat, aggregate_level = 'site') %>%
    select(site,sensor,date,treatment, get(stat)) %>%
    dcast(site + sensor + date ~ treatment, value.var = stat) %>%
    mutate(difference = OTC - CTL)
  min_date <- min(dat$date)
  max_date <- max(dat$date)
  
  no_otc_dates <- findInterval(dat$date, as.Date(t(chamber_dates[, 2:3])), rightmost.closed=TRUE)
  otc_on_dates <- filter(dat, (no_otc_dates %% 2) == 0)
  mean_diff <- data.frame(mean = mean(otc_on_dates$difference))
                           
  
  ggplot(dat, aes(x = date,y = difference), axis.line = element_line()) + 
    geom_rect(data = chamber_dates, aes(xmin = chambers_in, xmax = chambers_out, 
                                        ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes=FALSE) + 
    geom_path() +
    geom_hline(data = mean_diff, aes(yintercept = mean), col='red', alpha=0.6, size = 1) +
    geom_hline(aes(yintercept = 0), col='blue', alpha=0.6, size= 1) +
    scale_x_date(expand=c(0,0), limits =c(min_date,max_date)) +
    ylab(ylab) +
    partial_plot_theme(strips = FALSE)
}

microclimate_diff_plots <- function(hourly_microclimate, subset_site='ITEX2.0', stat, stat_label ='Mean') {
  p1 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CAmbient_Temp',
                                stat = stat,
                                ylab = substitute(bold(stat_label~'ambient temperature'~paste('(',degree, C,')')),list(stat_label=stat_label)))
  
  p2 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CTemp_3cmBG',
                                stat = stat,
                                ylab = substitute(bold(stat_label~'soil temperature'~paste('(',degree, C,')')),list(stat_label=stat_label)))
  
  p3 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CRH',
                                stat = stat,
                                ylab = substitute(bold(stat_label~'relative humidity (%)'),list(stat_label=stat_label)))
    

  p4 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CMoisture3to10cmBG',
                                stat = stat,
                                ylab = substitute(bold(stat_label~'volumetric water content (%)'),list(stat_label=stat_label)))
  
  grid.arrange(p1,p2,p3,p4, ncol=2)
      
}

