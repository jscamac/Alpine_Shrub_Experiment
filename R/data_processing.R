# Cleans raw inter-tussock poa distance data
clean_raw_censored_poa <- function(csv_file) {
  poadist <- read.csv(csv_file)
  names(poadist) <-tolower(names(poadist))
  
  poadist <- poadist %>%
    mutate(date = as.Date(as.character(poadist$date), format="%d/%m/%y"),
           may_sample = ifelse(month(date) >=4 & month(date) <=5, 1,0),
           otc = ifelse(treatment=='CTL',0,1),
           poadist = ifelse(poadist==0,0.5,poadist),
           is_censored = ifelse(is.na(poadist),1,0)) %>%
    filter(rep!='181GC' & mortality==0 & may_sample==1) %>% droplevels() %>%
    arrange(poadist,plot, rep, date) %>%
    select(plot,otc,ind=rep,date,poa_distance_cm=poadist,died=mortality,is_censored)
  return(poadist)
}

# Calculate median poa distance to each inter-tussock shrub seedling
calculate_median_poa_distances <- function(data, censored_model) {
  data %>%
    mutate(pred_dist = as.vector(get_posterior_mean(censored_model$fit, pars=c('full_data'))[,4]),
           id = as.numeric(factor(data$ind))) %>%
    group_by(otc, plot, id, ind, date) %>%
    summarise(median_poadist = median(pred_dist)) %>%
    mutate(initial_median_poadist = median_poadist[date=='2010-05-15'],
           years = as.vector(julian(date, as.Date("2010-05-15", "%Y-%m-%d")))/365.25) %>%
    select(otc,plot,id,date,years,initial_median_poadist,median_poadist)
}

dead_next_census <- function(status){
  if(length(status) > 1){
    i <- 1:(length(status)-1)} # if more than 1 obs
  else{
    i <- 0
  }
  as.numeric(c(status[i] == 0 & status[i+1] == 1, NA))
}

# Cleans and prepares otc seedling data
clean_seedling_data <- function(csv_file) {
  seedling_data <- read.csv(csv_file)
  names(seedling_data) <- tolower(names(seedling_data))
  
  seedling_data <- seedling_data %>%
    select(site,plot, otc=trt, inter_tussock=trt2, spp,ind=rep,census=samp,date=date,ht,
           fbd,bd,died=mortality) %>%
    mutate(community = ifelse(otc =='BCH','closed_heath','open_heath'),
           otc = ifelse(otc=='OTC', 1,0),
           spp = factor(spp, labels = c('Asterolasia', 'Grevillea','Phebalium','Prostanthera')),
           inter_tussock = ifelse(inter_tussock=='NORMAL', 0,1),
           date = as.Date(as.character(date), format="%d/%m/%y"),
           year = as.numeric(julian(date, as.Date("2010-05-15", "%Y-%m-%d")))/365.25, 
           may_sample = ifelse(month(date) >=4 & month(date) <=5, 1,0),
           ht = ht/10,
           fbd = fbd/10,
           bd = bd/10,
           stem_diam = (fbd + bd)/2) %>%
    arrange(ind,plot, date) %>%
    group_by(ind) %>% 
    mutate(only_1_census = max(ifelse(census==1 & died==1, 1,0))) %>%
    filter(only_1_census==0 & ind !='181GC' & community=='open_heath') %>% 
    droplevels() %>%# removes erroenous individual 181GC also removes individuals that did not survive for 2 censuses
    mutate(replaced = max(ifelse(is.na(ht[census==0]),1,0)),
           initial_ht = ifelse(replaced==1, ht[census==1], ht[census==0]),
           initial_fbd = ifelse(replaced==1, fbd[census==1], fbd[census==0]),
           initial_bd = ifelse(replaced==1, bd[census== 1], bd[census== 0]),
           initial_stem_diam = ifelse(replaced==1, stem_diam[census== 1], stem_diam[census== 0]),
           ht = ifelse(replaced==1 & census==0, initial_ht, ht),
           fbd = ifelse(replaced==1 & census==0, initial_fbd, fbd),
           bd = ifelse(replaced==1 & census==0, initial_bd, bd),
           stem_diam = ifelse(replaced==1 & census==0, initial_bd, stem_diam)) %>%
    ungroup() %>%
    select(site,community,plot,otc,inter_tussock,
           spp,ind,census,may_sample,date,year,ht,initial_ht,
           fbd,initial_fbd,bd,initial_bd,initial_stem_diam,stem_diam,died)
  return(seedling_data)
}

# merges predicted median poa distances with otc seedling data
merge_seedling_poa <- function(cleaned_seedling_data, median_poa_distances) {
  data <-merge(cleaned_seedling_data,median_poa_distances, by=c('plot','otc','ind','date'), all.x=TRUE)
  data <- select(data,site,community,plot,otc,inter_tussock,spp,ind,census,may_sample,
                 date,year,initial_median_poadist,median_poadist,initial_ht,ht,
                 initial_fbd,fbd,initial_bd,bd,initial_stem_diam,stem_diam,died)
  return(data)
}

# subsets otc growth data by inter_tussock type
get_growth_data <- function(full_seedling_data, inter_tussock = TRUE) {
  if(inter_tussock == TRUE) {
    filter(full_seedling_data, may_sample==1 & died==0 & inter_tussock==1) %>%
      droplevels() %>%
      select(site,community,plot,otc,spp,ind,census,date,year,
             median_poadist,initial_ht,ht, stem_diam)
  } else {
    filter(full_seedling_data, may_sample==1 & died==0 & inter_tussock==0) %>%
      droplevels() %>%
      select(site,community,plot,otc,spp,ind,census,date,year,initial_ht,ht,initial_stem_diam,stem_diam)
  }
}

# subsets otc mortality data by inter_tussock type
get_mortality_data <- function(complete_otc_seedling_data, inter_tussock=TRUE) {
  if(inter_tussock == TRUE) {
    complete_otc_seedling_data %>%
      filter(may_sample==1 & inter_tussock==1) %>%
      droplevels() %>%
      group_by(ind) %>%
      mutate(census_interval = c(diff(year),NA),
             dead_next_census = dead_next_census(died)) %>%
      ungroup() %>%
      filter(died==0 & !is.na(census_interval)) %>%
      select(site,community,plot,otc,spp,ind,census,date,year,ht,stem_diam,
             median_poadist,census_interval,dead_next_census)
  } else{
    complete_otc_seedling_data %>%
      filter(may_sample==1 & inter_tussock==0) %>%
      droplevels() %>%
      group_by(ind) %>%
      mutate(census_interval = c(diff(year),NA),
             dead_next_census = dead_next_census(died)) %>%
      ungroup() %>%
      filter(died==0 & !is.na(census_interval)) %>%
      droplevels() %>%
      select(site,community,plot,otc,spp,ind,census,date,year,ht,stem_diam,census_interval,dead_next_census)
  }
}

# Calculates average maximum height from multiple field studies
hmax_priors <- function(data) {
 data <- data %>%
   filter(!is.na(species)) %>%
   droplevels() %>%
   group_by(site,species) %>%
   summarise(max_ht = max(height_cm, na.rm=TRUE)) %>%
   ungroup() %>%
   group_by(species) %>%
   summarise(n = n(),
             mn_max_ht = round(mean(max_ht)),
             sd_max_ht = sd(max_ht),
             se_max_ht = sd_max_ht/sqrt(n)) %>%
   mutate(species = factor(species, labels= c('Asterolasia','Grevillea','Phebalium','Prostanthera')))
 return(data)
}

# Cleans unburnt field data       
clean_unburnt_data <- function(csv_file) {
  data <- read.csv(csv_file)
  data <- data %>%
    setNames(tolower(names(.))) %>%
    select(site, year, transect=tran, greaus_tran_adultden=greaus_adultden,
           plot, poa, bare_ground=bg, rock, species, adult, height, 
           diameter=stemdiam, canopy_length_1 = clength1, canopy_length_2 = clength2) %>%
    tidyr::complete(nesting(site,year,transect,greaus_tran_adultden,plot,poa,bare_ground,rock),species) %>%
    filter(!is.na(species)) %>%
    mutate(greaus_tran_adultden = greaus_tran_adultden/500) %>% # converts to density/m2
    droplevels()
  return(data)
}

# Summarises unburnt field data
summarise_unburnt_data <- function(data) {
 test<- suppressWarnings(data %>% 
    group_by(site,year,transect,greaus_tran_adultden,plot,poa,bare_ground,rock,species) %>%
    summarise(adult_density = sum(adult==1, na.rm=TRUE),
              seedling_density = sum(adult==0, na.rm = TRUE),
              max_seedling_height_cm = ifelse(!is.finite(max(height[adult==0], na.rm=TRUE)),NA, max(height[adult==0], na.rm=TRUE)),
              max_seedling_diameter_mm = ifelse(!is.finite(max(diameter[adult==0], na.rm=TRUE)),NA, max(diameter[adult==0], na.rm=TRUE)),
              max_seedling_canopy_area_cm = ifelse(!is.finite(max(pi * canopy_length_1[adult==0]/2 * canopy_length_2[adult==0]/2)),NA,
                                                   max(pi * canopy_length_1[adult==0]/2 * canopy_length_2[adult==0]/2)),
              max_adult_height_cm = ifelse(!is.finite(max(height[adult==1], na.rm=TRUE)),NA, max(height[adult==1], na.rm=TRUE)),
              max_adult_diameter_mm = ifelse(!is.finite(max(diameter[adult==1], na.rm=TRUE)),NA, max(diameter[adult==1], na.rm=TRUE)),
              max_adult_canopy_area_cm = ifelse(!is.finite(max(pi * canopy_length_1[adult==1]/2 * canopy_length_2[adult==1]/2)),NA,
                                                   max(pi * canopy_length_1[adult==1]/2 * canopy_length_2[adult==1]/2)))) %>%
    ungroup() %>%
    mutate(site = as.character(site))
}

# Cleans burnt density data
clean_burnt_density<- function(csv_file) {
  data <- read.csv(csv_file)
  data <- data %>%
    setNames(tolower(names(.))) %>%
    select(site, year, transect=tran, greaus_tran_adultden=greaus_adultden,
           plot, poa, bare_ground=bg, rock, species, seedling_density=sdldensity) %>%
    filter(species!="PROCUN") %>%
    droplevels() %>%
    mutate(site = as.character(site),
           adult_density=0,
           greaus_tran_adultden = greaus_tran_adultden/500) # converts to density/m2
  return(data)
}

# Cleans and summarises burnt height data
clean_burnt_ht <- function(csv_file) {
  data <- read.csv(csv_file)
  data <- data %>%
    setNames(tolower(names(.))) %>%
    select(site, year, transect=tran, greaus_tran_adultden=greaus_adultden,
           plot, poa, bare_ground=bg, rock, species, height_replicate=height.rep, height) %>%
    filter(species!="PROCUN") %>%
    droplevels() %>%
    group_by(site,year,transect,greaus_tran_adultden,plot,poa,bare_ground,rock,species) %>%
    summarise(max_seedling_height_cm = max(height, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(site = as.character(site),
           adult_density = 0,
           greaus_tran_adultden = greaus_tran_adultden/500) # converts to density/m2
  return(data)
}

# Merges burnt and unburnt data into a single dataframe
merge_all_quadrat_data <- function(summarised_unburnt_data,cleaned_burnt_density_data, summarised_burnt_height_data, site_vars) {
  data <- cleaned_burnt_density_data %>%
    full_join(summarised_burnt_height_data) %>%
    mutate(adult = 0) %>%
    full_join(summarised_unburnt_data) %>%
    mutate(site = as.factor(site),
           species = factor(species, labels = c('Asterolasia','Grevillea','Phebalium'))) %>%
    full_join(site_vars) %>%
    select(site,longitude,latitude,altitude_m,burnt_03,min_twig_diam,topo_wetness_index,
           year,transect,greaus_tran_adultden,plot,poa,bare_ground,rock,spp = species,species,seedling_density,
           adult_density,max_seedling_height_cm, max_seedling_diameter_mm,max_seedling_canopy_area_cm,
           max_adult_height_cm, max_adult_diameter_mm,max_adult_canopy_area_cm) %>%
    mutate(spp = factor(spp, levels = c('Asterolasia','Grevillea','Phebalium'))) %>%
    arrange(site, spp)
  return(data)
}

# Save function for exporting remake targets to hard drive
export_data <- function(data, filename) {
  saveRDS(data, filename)
  filename_fmt <- sub("\\.rds$", "_%s.rds", filename)
  filename_sub <- sprintf(filename_fmt, seq_along(data))
  for (i in seq_along(data)) {
    saveRDS(data[[i]], filename_sub[[i]])
  }
}

# Summarise height observations
summarise_otc_growth_obs <- function(observed_data, response ='ht') {
  observed_data %>%
    mutate(otc = factor(otc,labels = c('ctl','otc'))) %>%
    group_by(otc, spp, date) %>%
    summarise_(n = ~n(),
              ln_mn = lazyeval::interp(~mean(log(response)), response = as.symbol(response)),
              ln_sd = lazyeval::interp(~sd(log(response)), response = as.symbol(response))) %>%
    mutate(ln_ci = 1.96* (ln_sd/sqrt(n)),
           ln_lower_95_ci = ln_mn - ln_ci,
           ln_upper_95_ci = ln_mn + ln_ci) %>%
  ungroup %>%
    mutate(mean = exp(ln_mn),
           lower_95ci = exp(ln_lower_95_ci),
           upper_95ci = exp(ln_upper_95_ci)) %>%
    select(otc, spp, date, n, mean, lower_95ci, upper_95ci)
}

# Calculates density and height model covariate distributions
density_height_covariate_summary <- function(data) {
  site_covs <- c('site', 'min_twig_diam','altitude_m','topo_wetness_index')
  tran_covs <- c('site','transect', 'greaus_tran_adultden')
  site_data <- unique(data[, site_covs]) %>% select(-site)
  tran_data <- unique(data[, tran_covs]) %>% select(c(-site,-transect))
  
  site_data %>%
    tidyr::gather(covariate) %>%
    group_by(covariate) %>%
    summarise(mean = mean(value, na.rm=TRUE),
              sd = sd(value, na.rm=TRUE),
              min = min(value, na.rm=TRUE),
              max = max(value, na.rm=TRUE))
  s <- apply(site_data, 2, function(x) {
    rbind(c(
      mn = mean(x, na.rm=TRUE),
      std = sd(x, na.rm=TRUE),
      min = min(x, na.rm=TRUE),
      max = max(x, na.rm=TRUE)))
  })
  
  t <- apply(tran_data, 2, function(x) {
    rbind(c(
      mn = mean(x, na.rm=TRUE),
      std = sd(x, na.rm=TRUE),
      min = min(x, na.rm=TRUE),
      max = max(x, na.rm=TRUE)))
  })
  
  res <- cbind.data.frame(s,t)
  rownames(res) <- c('mean','sd','min','max')
  return(res)
}

# Creates vectors of covariate values between observed range
simulate_covariate_range <- function(data) {
  dat <- density_height_covariate_summary(data)
  cbind.data.frame(sim_severity = seq(round(dat['min','min_twig_diam']),round(dat['max','min_twig_diam']),length.out = 100),
                   sim_altitude = seq(round(dat['min','altitude_m'],-1),round(dat['max','altitude_m'],-1),length.out = 100),
                   sim_twi =  seq((dat['min','topo_wetness_index']),round(dat['max','topo_wetness_index']),length.out = 100),
                   sim_adult_den = seq(round(dat['min','greaus_tran_adultden'],1),round(dat['max','greaus_tran_adultden'],1),length.out = 100))
}

# Process climate data

process_hourly_microclimate <- function(rds_file_path, meta_data_path, subset_site=NULL, include_raw_data=FALSE) {
  dat <- readRDS(rds_file_path)
  metadat <- read.csv(meta_data_path)
  
  if(!all(unique(dat$Site) %in% metadat$Site)) {
    stop("Please check that site is spelt correctly for all observations. Only ITEX1U or ITEX2.0 accepted")
  }
  
  if(!all(do.call(paste, unique(dat[, c('Site', 'Plot', 'Treatment')])) %in% 
          do.call(paste, unique(metadat[, c('Site', 'Plot', 'Treatment')])))) {
    stop("Please check that treatment is correctly assigned to all plots")
  }
  
  if(!is.null(subset_site)){
    dat <- filter(dat, Site==subset_site)
    metadat <- filter(metadat, Site==subset_site)
  }
  metalogger <- metadat[which(metadat$Microstation==1),]
  if(!all(unique(paste0(dat$Site,dat$Plot)) %in% paste0(metalogger$Site,metalogger$Plot))) {
    stop("Some observations are from plots with no logger. Please double check data. If new logger installed, please add to the metadata file")
  }
  
  # Below adds a datetime field. This corrects times according to daylight savings. 
  # The +1000 declares that the times are currently in UTC+1000 (i.e. AEST).
  # It also removes data that is errenous due to loggers failing or battery power being too low.
  # Plot 6 has been malfunctioning since October 2011. As such the data collected from this logger is deemed unusable.
  # Plot 2 and 19 have had on going battery issues. Most of the data recorded appears correct but occassionally
  # the day before the logger batteries die the sensors error.
  # Processing rules are based on minimum and maximum temperatures observed from other nearby sites & expert judgement.
  # We purposely didn't make the rules too strict so that rare extremes were not removed.
  dat <- dat %>%
    mutate(Datetime = fast_strptime(paste(Date, hour(hms(as.character(Time))), '+1000'), '%d/%m/%y %H %z', tz="Australia/Melbourne"),
           Date = as.Date(Datetime),
           Plot = as.integer(as.character(Plot)),
           Time = NULL,
           CAmbient_Temp = ifelse(Ambient_Temp > 50 |
                                    Ambient_Temp < - 30 |
                                    Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                    Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                    Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                    Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                                  NA, Ambient_Temp),
           CTemp_3cmBG = ifelse(Temp_3cmBG > 80 | 
                                  Temp_3cmBG < -30 |
                                  Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                  Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                  Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                  Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                  Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                  Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                  Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                                NA, Temp_3cmBG),
           CRH = ifelse(Ambient_Temp > 50 |
                          Ambient_Temp < -30 |
                          Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                          Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                          Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                          Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                          Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                          Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                          Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,
                        NA, RH),
           CDewPt = ifelse(DewPt < -50 |
                             DewPt > 50 |
                             Ambient_Temp > 50 |
                             Ambient_Temp < -30 |
                             Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                             Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                             Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                             Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                             Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                             Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                             Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                           NA, DewPt),
           CMoisture_3cmBG = ifelse(Moisture_3cmBG < 0 |
                                      Moisture_3cmBG > 1 |
                                      Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                      Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                      Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                      Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                      Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                      Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                      Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,  
                                    NA, Moisture_3cmBG*100),
           CMoisture_10cmBG = ifelse(Moisture_10cmBG < 0 |
                                       Moisture_10cmBG > 1 |
                                       Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                       Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                       Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                       Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                       Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                       Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                       Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,
                                     NA, Moisture_10cmBG*100),
           CMoisture3to10cmBG = (CMoisture_3cmBG + CMoisture_10cmBG)/2) %>%
    gather(Sensor, Clim_value, -Site, -Plot, -Treatment, -Datetime,-Date) %>%
    group_by(Site, Plot, Treatment, Datetime, Sensor) %>%
    summarise(Clim_value = mean(Clim_value, na.rm=TRUE)) %>%
    ungroup()
  
  all_dates <- data.frame(Datetime = seq.POSIXt(range(dat$Datetime)[1], range(dat$Datetime)[2], 'hour', 
                                                tz='Australia/Melbourne'))
  
  meta <- select(metalogger, Site, Plot, Treatment)
  meta <- meta[rep(seq_len(nrow(meta)), each=nrow(all_dates)),] %>%
    mutate(Datetime = rep(all_dates$Datetime, nrow(metalogger)))
  
  meta <- meta[rep(seq_len(nrow(meta)), each=length(unique(dat$Sensor))),] %>%
    mutate(Sensor = rep(unique(dat$Sensor), length.out=n()))
  
  complete <- left_join(meta, dat) %>%
    mutate(
      Date = as.Date(Datetime),
      Hour = hour(Datetime),
      Clim_value = ifelse(is.nan(Clim_value),NA,Clim_value)) %>%
    select(Site, Plot,Treatment,Datetime,Date,Hour,Sensor,Clim_value) %>%
    arrange(Site,Sensor,Plot, Datetime) %>%
    setNames(tolower(names(.)))
  
  if(include_raw_data==TRUE){
    complete
  }
  else {
    filter(complete, !sensor %in% c('Ambient_Temp','RH','DewPt', 'Temp_3cmBG','Moisture_3cmBG', 'Moisture_10cmBG'))
  }
}

# Max and Min functions that return NA instead of -inf when no observations present.
mymax <- function(...,na.rm=FALSE) {
  if(!is.infinite(x<-suppressWarnings(max(...,na.rm=na.rm)))) x else NA
}
mymin <- function(...,na.rm=FALSE) {
  if(!is.infinite(x<-suppressWarnings(min(...,na.rm=na.rm)))) x else NA
}

# Microclimate aggregation function

aggregate_microclimate_to_day <- function(hourly_microclimate, aggregate_level = 'site') {
  
  if(!aggregate_level %in% c('site','plot','treatment')) {
    stop("Can only aggregate to 'site','plot','treatment'")
  }
  
  # Calculates plot averages
  if(aggregate_level =='plot'){
    res <- hourly_microclimate %>% 
      group_by(site,plot,treatment,sensor,date) %>%
      summarise(n = sum(!is.na(clim_value)),
                mean = mean(clim_value, na.rm = TRUE),
                min = mymin(clim_value, na.rm = TRUE),
                max = mymax(clim_value, na.rm = TRUE)) %>%
      ungroup()
  }
  # Calculates treatment averages for each site (if site is not subsetted)
  # Uses plot means - as plots are the unit of replication
  if(aggregate_level =='site'){
    res <- hourly_microclimate %>% 
      group_by(site,plot,treatment,sensor,date) %>%
      summarise(mean = mean(clim_value, na.rm = TRUE),
                min = mymin(clim_value, na.rm = TRUE),
                max = mymax(clim_value, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(site,treatment,sensor,date) %>%
      summarise(n = sum(!is.na(mean)),
                mean = mean(mean,na.rm=TRUE), 
                mean_min = mymin(min, na.rm=TRUE), 
                mean_max = mymax(max, na.rm=TRUE)) %>%
      ungroup()
  }
  return(res)
}

calculate_overall_treatment_effects <- function(hourly_microclimate, calculate_trt_difference = TRUE) {
  chamber_dates <- readRDS('raw_data/chamber_dates.rds')
  no_otc_dates <- findInterval(hourly_microclimate$date, as.Date(t(chamber_dates[, 2:3])), rightmost.closed=TRUE)
  hourly_microclimate <- filter(hourly_microclimate, (no_otc_dates %% 2) == 0) # removes periods when no otcs were not on
  
  dat <- aggregate_microclimate_to_day(hourly_microclimate,aggregate_level = 'site')
  
  if(calculate_trt_difference == TRUE) {
    res <- dat %>% select(site,treatment,sensor, date, mean, mean_min, mean_max) %>%
      gather(clim_stat, value, -site,-treatment,-sensor,-date) %>%
      spread(treatment, value) %>%
      mutate(difference = OTC - CTL) %>%
      group_by(site,sensor, clim_stat) %>%
      summarise(grand_mean = mean(difference, na.rm = TRUE),
                st_err = sd(difference, na.rm = TRUE) /sqrt(sum(!is.na(difference))),
                ci = 1.96 * st_err,
                `2.5%` = grand_mean - ci,
                `97.5%` = grand_mean + ci)
  } else {
    res <- dat %>% select(site, treatment,sensor, date, mean, mean_min, mean_max) %>%
      gather(clim_stat, value, -site,-treatment,-sensor,-date) %>%
      group_by(site,sensor, clim_stat) %>%
      summarise(grand_mean = mean(value, na.rm = TRUE),
                st_err = sd(value, na.rm = TRUE) /sqrt(n()),
                ci = 1.96 * st_err,
                `2.5%` = grand_mean - ci,
                `97.5%` = grand_mean + ci)
  }
  return(res)
}
  
  