microstation <-readRDS('raw_data/otc_microstation_data.rds')


import_microstation <- function(microstation, filename) {
  require(data.table)
  #microstation <- fread(microstation)
  d <- fread(filename)
  #d[, `#`:=NULL]
  
  site <- factor(sub('_.*', '', basename(filename)))
  plot <- factor(gsub('^.*_|\\D', '', basename(filename)))
  treatment <- factor(sub('.*(.{3})\\.csv', '\\1', basename(filename)))

  d[, c('Site', 'Plot', 'Treatment'):=list(site, plot, treatment)]
  
  d2 <- d[, unlist(lapply(c('Site', 'Plot', 'Treatment', 'Date', 
                            'Time', 'Ambient', 'RH', 'DewPt', 
                            '3cmBG', '5cmBG', '10cmBG'), 
                          grep, colnames(d))), with=FALSE]

  setnames(d2, names(microstation)[
    unlist(lapply(c('Site', 'Plot', 'Treatment', 'Date', 'Time', 
                    'Ambient', 'RH', 'DewPt', '3cmBG', '5cmBG', 
                    '10cmBG'), 
                  function(x) any(grepl(x, colnames(d)))))])
  d2
  #   rbind(microstation, d2, fill=TRUE)
}

# z <- fun(microstation, '~/Desktop/ITEX1U_1OTC.csv')
ff <- list.files('~/Dropbox/Projects/Oz_Alps/Other_datasets/ITEX/ITEX climate/Needs_to_be_entered/micros/may16_microloggers/', patt='OTC\\.csv|CTL\\.csv', full.names=TRUE)
z <- do.call(rbind, c(lapply(ff, import_microstation, microstation=microstation), list(fill=TRUE)))
microstation <- as.data.frame(rbind(microstation, z, fill=TRUE))

saveRDS(microstation, file='raw_data/otc_microstation_data.rds')
