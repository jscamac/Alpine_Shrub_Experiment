
# Theme for partial plots
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey', size=0.7) else element_blank()
  st <- if(strips==TRUE) element_text(face='italic') else element_blank()
  theme_classic(base_size = 7) + theme(strip.text = st,
                          legend.title = element_blank(),
                          strip.background = sb,
                          legend.position = legend.position,
                          plot.margin = unit(c(3,3,3,3), "mm"),
                          axis.text = element_text(colour="black"),
                          axis.line = element_blank(),
                          panel.border = element_rect(fill=NA, colour ="black"),
                          axis.ticks = element_line(size =0.3, colour="black"),
                          axis.ticks.length=unit(-0.7, "mm"),
                          axis.text.x = element_text(margin=unit(c(2,0,0,0), "mm")),
                          axis.text.y = element_text(margin=unit(c(0,2,0,0), "mm")))
}

# Theme for coefficient plots
coefficent_plot_theme <- function() {
  theme_classic(base_size = 7) + theme(
                          axis.title.y = element_blank(),
                          plot.margin = unit(c(3,3,3,3), "mm"),
                          axis.line = element_blank(),
                          axis.title = element_text(margin=c(0,-0.5,0,0)),
                          axis.text = element_text(colour="black"),
                          panel.border = element_rect(fill=NA, colour ="black"),
                          axis.ticks = element_line(size =0.3, colour="black"),
                          axis.ticks.length=unit(-0.7, "mm"),
                          axis.text.x = element_text(margin=unit(c(2,0,0,0), "mm")),
                          axis.text.y = element_text(margin=unit(c(0,2,0,0), "mm")))
}


# Plot observed height growth
plot_obs_growth <- function(observed_data, response ='ht', scales ='free', ncol=4, 
                            ylab = 'Observed height (cm)', 
                            xlab ='Observation year') {
  
  summary_obs <- summarise_otc_growth_obs(observed_data, response)
  observed_data$otc <- factor(ifelse(observed_data$otc==1, 'otc','ctl'))
  ylim <- range(pretty(c(0, max(observed_data[[response]]))))
  
  ggplot() + 
  geom_path(data = observed_data, aes_string(x = 'date',y = response, colour='otc', group = 'ind'), alpha = 0.4, size=0.3) +
  geom_path(data = summary_obs, aes(x=date, y=mean, col=factor(paste0(otc, '_mean')), linetype=otc), alpha=0.6) +
  geom_linerange(data= summary_obs,aes(x=date,ymin = lower_95ci, ymax=upper_95ci, col=factor(paste0(otc, '_mean')))) +
  geom_point(data= summary_obs,aes(x=date, y=mean, col=factor(paste0(otc, '_mean')),
                                     fill=factor(paste0(otc, '_mean'))), shape = 21) +      
  scale_colour_manual(values = c("ctl" ="sky blue","otc" ="orange", "otc_mean"="red", "ctl_mean"="blue")) +
  scale_fill_manual(values = c("ctl_mean" ="white","otc_mean" ="red")) +
  scale_linetype_manual(values= c("ctl" ="dashed","otc" ="solid")) +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
    partial_plot_theme(strips = TRUE) +
    ylim(ylim) +
    ylab(ylab) + 
    xlab(xlab) + 
    facet_wrap(~spp,ncol=ncol, scale=scales) 
}

# Plot regression coefficients
coefficient_plot <- function(summarised_coefficients, y_axis_labels='',xlab ='Effect size', title=NULL) {
  
  
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter))+
    geom_point(aes(x=mean, y=parameter)) +
    geom_vline(aes(xintercept=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(4)) +
    labs(title=title) +
    coefficent_plot_theme()
}


# Plot predicted height growth
plot_growth_curves <- function(non_tussock_otc_growth_model, scales ='free', ncol=4, ylab = 'Projected height (cm)', xlab ='Year') {
  predictions <- summarise_otc_model_predictions('non_tussock_growth',non_tussock_otc_growth_model)
  vert_line <- extract_hmax_year(predictions)
  
  ggplot(predictions, aes(x = pred_year,y = mean, colour = as.factor(treatment), fill = as.factor(treatment)), axis.line = element_line()) + 
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    scale_fill_manual(values= c('ctl' ='blue','otc'='red')) +
    geom_line(aes(linetype=as.factor(treatment))) +
    scale_linetype_manual(values= c('ctl' ='longdash','otc'='solid')) +
    scale_color_manual(values= c('ctl' ='blue','otc'='red')) +
    geom_segment(data=vert_line,aes(x=pred_year,y=hmax, xend=pred_year, yend=-Inf),linetype="dotted", na.rm=TRUE) +
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
    geom_line(aes(linetype=as.factor(treatment))) +
    scale_linetype_manual(values= c('ctl' ='longdash','otc'='solid')) +
    scale_colour_manual(values= c('ctl' ='blue','otc' ='red')) +
    scale_fill_manual(values= c('ctl' ='blue','otc' ='red')) +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    ylab(ylab) + 
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) +
    partial_plot_theme(strips=FALSE)
}

# Predicted mortality in OTC vs CTL for non tussock seedlings
non_tussock_mortality_plot <- function(non_tussock_mortality_model, ylab =expression('Probability of death'~(yr^{-1})), xlab ='Species') {
  mortality <- summarise_otc_model_predictions('non_tussock_mortality',non_tussock_mortality_model)
  
  
  ggplot(mortality, aes(x = spp,y = mean, group= treatment, colour=treatment, fill=treatment)) + 
    geom_linerange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.2)) +
    geom_point(shape = 21, position=position_dodge(.2)) +
    scale_colour_manual(values = c("ctl" ="blue","otc" ="red")) +
    scale_fill_manual(values = c("ctl" ="white","otc" ="red")) +
    ylab(ylab) + 
    xlab(xlab) +
    partial_plot_theme() + 
    theme(axis.text.x = element_text(face='italic'))
}

# Predicted number of natural recruits in OTC vs CTL for first two seasons
recruit_plot <- function(recruit_model, ylab =expression('Recruits'~(m^{-2})), xlab ='Census') {
  recruits <- summarise_otc_model_predictions('recruits',recruit_model)
  recruits$census <- as.factor(rep(c('2011', '2012'),2))
  
  
  ggplot(recruits, aes(x = census,y = mean, group= treatment, colour=treatment, fill=treatment)) + 
    geom_linerange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.2)) +
    geom_point(shape=21, position=position_dodge(.2)) +
    scale_colour_manual(values = c("ctl" ="blue","otc" ="red")) +
    scale_fill_manual(values = c("ctl" ="white","otc" ="red")) +
    ylab(ylab) + 
    xlab(xlab) +
    partial_plot_theme()
}


# Plots covariate partial plots for density and height models
partial_plot_density_height <- function(predictions, x, xlab=NULL,ylab=NULL, ylim =c(0,40)) {
  ggplot(predictions, aes_string(x = x,y = 'mean'), axis.line = element_line()) + 
    geom_line() +
    scale_fill_manual(values= 'grey60') +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    scale_x_continuous(expand=c(0,0.001)) +
    ylim(ylim) +
    xlab(xlab) +
    ylab(ylab) +
    partial_plot_theme(strips=FALSE)
}

burnt_v_unburnt <- function(predictions, x, ylab=NULL) {
  ggplot(predictions, aes_string(x = x,y = 'mean')) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.3) +
    scale_x_discrete(labels =c('Unburnt','Burnt')) +
    xlab('') +
    ylab(ylab) +
    partial_plot_theme()
}

gap_dynamics_curve <- function(summarised_predictions, xlab='Year',ylab='Inter-tussock gap radius (cm)',ylim=c(0,15)) {
  ggplot(summarised_predictions, aes(x = pred_yr,y = mean, group=as.factor(treatment), colour = as.factor(treatment), fill = as.factor(treatment)), axis.line = element_line()) + 
    scale_colour_manual(values= c('ctl' ='blue','otc' ='red')) +
    scale_fill_manual(values= c('ctl' ='blue','otc' ='red')) +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) + 
    geom_line(aes(linetype=as.factor(treatment))) +
    scale_linetype_manual(values= c('ctl' ='longdash','otc'='solid')) +
    ylab(ylab) + 
    ylim(ylim) +
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) +
    partial_plot_theme(strips=FALSE)
}

# PANEL PLOTS

# Observed and predicted growth curves
obs_pred_ht_growth <- function(observed_data, non_tussock_otc_growth_model) {
  p1 <- plot_obs_growth(observed_data,'ht')
  p2 <- plot_growth_curves(non_tussock_otc_growth_model)
  print(plot_grid(p1,p2, labels=paste0('(',letters[1:2],')'), ncol = 1, label_size = 7),vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
}
  
# Partial plots for growth and mortality against poa distance size
tussock_plots <- function(tussock_growth_model, tussock_mortality_model) {
  growth <- summarise_coefficients(tussock_growth_model, c('alpha','b_otc','b_poadist','b_otc_x_poadist'))
  mortality <- summarise_coefficients(tussock_mortality_model, c('alpha','b_otc','b_poadist','b_otc_x_poadist'))
  growth_preds <- summarise_otc_model_predictions('tussock_growth',tussock_growth_model)
  mortality_preds <- summarise_otc_model_predictions('tussock_mortality',tussock_mortality_model)
  p1 <- coefficient_plot(growth, y_axis_labels = c('Intercept','Warmed','Gap','Warmed x Gap'), 
                         xlab = 'Growth coefficients')
  p2 <- poadist_plot(growth_preds,xlab = 'Inter-tussock gap radius (cm)',ylab = 'Logistic growth rate (R)')
  p3 <- coefficient_plot(mortality, y_axis_labels = c('Intercept','Warmed','Gap','Warmed x Gap'),
                         xlab = 'Mortality coefficents')
  
  p4 <- poadist_plot(mortality_preds, xlab = 'Inter-tussock gap radius (cm)',ylab = expression('Probability of death'~(yr^{-1})))
  plot_grid(p1,p2,p3,p4, labels=paste0('(',letters[1:4],')'), ncol = 2, label_size = 7)
}

density_count_plots <- function(density_model, species, ylim=c(0,35)) {
  if(species=='Grevillea') {
    params <- c('alpha_mu','b_unburnt','b_severity','b_altitude','b_twi','b_adult_density')
    y_axis_labels <- c('Intercept','Unburnt','MTD','Altitude','TWI','Adult density')
  }
  if(species=='Asterolasia'){
    params <- c('alpha_mu','b_unburnt','b_severity','b_altitude','b_twi')
    y_axis_labels <- c('Intercept','Unburnt','MTD','Altitude','TWI')
  }
  coeffs <- summarise_coefficients(density_model,params = params)
  predictions <- summarise_density_predictions(density_model,species)
  p1 <- coefficient_plot(coeffs,y_axis_labels,xlab = 'log coefficients')
  p2 <- burnt_v_unburnt(predictions$burnt_unburnt,x ='predictor',ylab = expression('Seedlings'~(m^{-2})))
  p3 <- partial_plot_density_height(predictions$severity, x ='sim_severity', 
                                    xlab ='MTD (mm)', ylab =expression('Seedlings'~(m^{-2})),ylim)
  p4 <- partial_plot_density_height(predictions$altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab =expression('Seedlings'~(m^{-2})),ylim) 
  p5 <- partial_plot_density_height(predictions$twi, x ='sim_twi', 
                                    xlab ='TWI', ylab =expression('Seedlings'~(m^{-2})),ylim)
  
  if(species=='Grevillea') {
    p6 <- partial_plot_density_height(predictions$adult_density, x ='sim_adult_den', 
                                      xlab = expression('Adults'~(m^{-2})), ylab = expression('Seedlings'~(m^{-2})), ylim)
    plot_grid(p1,p2,p3,p4,p5,p6, labels=paste0('(',letters[1:6],')'), ncol = 2, label_size = 8)
  }
  
  else{
      plot_grid(p1,p2,p3,p4,p5, labels=paste0('(',letters[1:5],')'), ncol = 2, label_size = 8)
  }
}


max_ht_plots <- function(greaus_max_ht_model,asttry_max_ht_model, ylim=c(0,35)) {
  
  coeffs_greaus <- summarise_coefficients(greaus_max_ht_model,params = c('mu_log_site','log_b_severity','log_b_altitude','log_b_twi'))
  predictions_greaus <- summarise_max_ht_predictions(greaus_max_ht_model)
  
  coeffs_asttry <- summarise_coefficients(asttry_max_ht_model,params = c('mu_log_site','log_b_severity','log_b_altitude','log_b_twi'))
  predictions_asttry <- summarise_max_ht_predictions(asttry_max_ht_model)
  
  p1 <- coefficient_plot(coeffs_asttry,c('Intercept','MTD','Altitude','TWI'),xlab = 'log coefficients', title=expression(italic('Asterolasia')))
  p2 <- coefficient_plot(coeffs_greaus,c('Intercept','MTD','Altitude','TWI'),xlab = 'log coefficients', title=expression(italic('Grevillea')))
  p3 <- partial_plot_density_height(predictions_asttry$pred_ht_severity, x ='sim_severity', 
                                    xlab ='MTD (mm)', ylab ='Max height (cm)',ylim)
  p4 <- partial_plot_density_height(predictions_greaus$pred_ht_severity, x ='sim_severity', 
                                    xlab ='MTD (mm)', ylab ='Max height (cm)',ylim)
  p5 <- partial_plot_density_height(predictions_asttry$pred_ht_altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab ='Max height (cm)',ylim) 
  p6 <- partial_plot_density_height(predictions_greaus$pred_ht_altitude, x ='sim_altitude', 
                                    xlab ='Altitude (m)', ylab ='Max height (cm)',ylim) 
  p7 <- partial_plot_density_height(predictions_asttry$pred_ht_twi, x ='sim_twi', 
                                    xlab ='TWI', ylab ='Max height (cm)',ylim)
  p8 <- partial_plot_density_height(predictions_greaus$pred_ht_twi, x ='sim_twi', 
                                    xlab ='TWI', ylab ='Max height (cm)',ylim)
  plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, labels=paste0('(',letters[1:8],')'), ncol = 2, label_size = 7, hjust=0.001)
}

gap_dynamics_plot <- function(gap_dynamic_model) {
  coeffs <- summarise_coefficients(gap_dynamic_model, params = c('alpha','b_otc'))
  predictions <- summarise_otc_model_predictions('gap_dynamics',gap_dynamic_model)
  p1 <- coefficient_plot(coeffs,y_axis_labels = c('Intercept', 'Warmed'), xlab = 'log coefficients')
  p2 <- gap_dynamics_curve(predictions)
  plot_grid(p1,p2, labels=paste0('(',letters[1:2],')'), ncol = 2, label_size = 7)
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
    select(site,sensor,date,treatment, stat) %>%
    dcast(site + sensor + date ~ treatment, value.var = stat) %>%
    mutate(difference = OTC - CTL)
  min_date <- min(dat$date)
  max_date <- max(dat$date)
  
  no_otc_dates <- findInterval(dat$date, na.omit(as.Date(t(chamber_dates[, 2:3]))), rightmost.closed=TRUE)
  otc_on_dates <- filter(dat, (no_otc_dates %% 2) == 0)
  mean_diff <- data.frame(mean = mean(otc_on_dates$difference,na.rm=TRUE)) # remove NA's for missing dates
                           
  
  ggplot(dat, aes(x = date,y = difference), axis.line = element_line()) + 
    geom_rect(data = chamber_dates, aes(xmin = chambers_in, xmax = chambers_out, 
                                        ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes=FALSE, na.rm=TRUE) + 
    geom_path(na.rm=TRUE) +
    geom_hline(data = mean_diff, aes(yintercept = mean), col='red', alpha=0.6, size = 1) +
    geom_hline(aes(yintercept = 0), col='blue', alpha=0.6, size= 1) +
    scale_x_date(expand=c(0,0), limits =c(min_date,max_date)) +
    xlab('Year') +
    ylab(ylab) +
    partial_plot_theme(strips = FALSE)
}

microclimate_diff_plots <- function(hourly_microclimate, subset_site='ITEX2.0', stat, stat_label ='Mean') {
  p1 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CAmbient_Temp',
                                stat = stat,
                                ylab = substitute(stat_label~'ambient temperature'~paste('(',degree, C,')'),list(stat_label=stat_label)))
  
  p2 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CTemp_3cmBG',
                                stat = stat,
                                ylab = substitute(stat_label~'soil temperature'~paste('(',degree, C,')'),list(stat_label=stat_label)))
  
  p3 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CRH',
                                stat = stat,
                                ylab = substitute(stat_label~'relative humidity (%)',list(stat_label=stat_label)))
    

  p4 <- plot_microclim_trt_diff(hourly_microclimate, 
                                subset_sensor = 'CMoisture3to10cmBG',
                                stat = stat,
                                ylab = substitute(stat_label~'volumetric water content (%)',list(stat_label=stat_label)))
  
  plot_grid(p1,p2,p3,p4, labels=paste0('(',letters[1:4],')'), ncol = 2, label_size = 7)
      
}

