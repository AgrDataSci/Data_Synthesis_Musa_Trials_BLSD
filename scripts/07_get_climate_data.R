library(climatrends)
library(ag5Tools)


#load data
musa_trials_data <- read.csv("data/processed/musa_trials_data.csv", row.names = 1, check.names = FALSE)

musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1)

head(musa_trials_data)

head(musa_trials_time_loc)

tail(musa_trials_time_loc)

min(musa_trials_time_loc$pdate)

max(musa_trials_time_loc$end_date)

nrow(musa_trials_time_loc)


musa_trials_prec <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                          start_date = "pdate",
                                          end_date = "end_date",
                                          lon = "lon",
                                          lat = "lat",
                                          variable = "Precipitation-Flux",
                                          path = "D:/Dropbox (Bioversity CR)/env_data/agera5/",
                                          ncores = 6) 

save(musa_trials_prec, file = "data/climate/musa_trials_prec.rda")

#load(file = "data/climate/musa_trials_prec.rda")

musa_trials_prec_ind <- vector(mode = "list", length = length(musa_trials_prec))

for(i in 1:length(musa_trials_prec)){
  
  musa_trials_prec_ind[[i]] <- climatrends::rainfall(object  = data.matrix(musa_trials_prec[[i]]),
                                                     day.one = colnames(musa_trials_prec[[i]])[1],
                                                     span    = length(musa_trials_prec[[i]]))
  
  
}


musa_trials_prec_ind <- do.call(rbind, musa_trials_prec_ind)



#temperature data
musa_trials_temp_daytime_max <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                                      start_date = "pdate",
                                                      end_date = "end_date",
                                                      lon = "lon",
                                                      lat = "lat",
                                                      variable = "Temperature-Air-2m",
                                                      statistic = "Max-Day-Time",
                                                      path = "D:/Dropbox (Bioversity CR)/env_data/agera5/",
                                                      celsius = TRUE,
                                                      ncores = 6)

#save(musa_trials_temp_daytime_max, file = "data/climate/musa_trials_temp_daytime_max.rda")


musa_trials_temp_nighttime_min <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                                        start_date = "pdate",
                                                        end_date = "end_date",
                                                        lon = "lon",
                                                        lat = "lat",
                                                        variable = "Temperature-Air-2m",
                                                        statistic = "Min-Night-Time",
                                                        path = "D:/Dropbox (Bioversity CR)/env_data/agera5/",
                                                        celsius = TRUE,
                                                        ncores = 6) 

#save(musa_trials_temp_nighttime_min, file = "data/climate/musa_trials_temp_nighttime_min.rda")

musa_trials_temp_ind <- vector(mode = "list", length = length(musa_trials_temp_daytime_max))

for(i in 1:length(musa_trials_temp_daytime_max)){
  
  musa_trials_temp_ind[[i]] <- temperature(as.numeric(musa_trials_temp_daytime_max[[i]][1, ]), 
                                           as.numeric(musa_trials_temp_nighttime_min[[i]][1, ]),
                                           span = length((musa_trials_temp_daytime_max[[i]][1,])))
}
musa_trials_temp_ind
musa_trials_temp_ind <- do.call(rbind, musa_trials_temp_ind)
musa_trials_temp_ind


musa_trials_climate <- cbind("id" = row.names(musa_trials_time_loc),
                             musa_trials_prec_ind,
                             musa_trials_temp_ind)

#write.csv(musa_trials_climate, file = "data/climate/musa_trials_climate.csv", row.names = FALSE)

#Relative humidity
#06h

musa_trials_rhum_06h <- ag5_extract(coords = musa_trials_time_loc,
                                    start_date = "pdate",
                                    variable = "Relative-Humidity-2m",
                                    time = "06h",
                                    path = "F:/env/AgERA5/rhum/",
                                    ncores = 4)


save(musa_trials_rhum_06h, file = "data/climate/musa_trials_rhum_06h.rda")


musa_trials_rhum_06h_mean <- lapply(musa_trials_rhum_06h, function(X) mean(as.numeric(X)))

musa_trials_rhum_06h_mean <- data.frame("rhum_06" = unlist(musa_trials_rhum_06h_mean))

#write.csv(musa_trials_rhum_06h_mean, file = "data/climate/musa_trials_rhum_06h_mean.csv")

as.numeric(musa_trials_rhum_06h[[1]])

#-----------------------------------------------------------------

musa_trials_rhum_09h <- ag5_extract(coords = musa_trials_time_loc,
                                   start_date = "pdate",
                                   variable = "Relative-Humidity-2m",
                                   time = "09h",
                                   path = "F:/env/AgERA5/rhum/",
                                   ncores = 4)


save(musa_trials_rhum_09h, file = "data/climate/musa_trials_rhum_09h.rda")


musa_trials_rhum_09h_mean <- lapply(musa_trials_rhum_09h, function(X) mean(as.numeric(X)))

musa_trials_rhum_09h_mean <- data.frame("rhum_09" = unlist(musa_trials_rhum_09h_mean))

#write.csv(musa_trials_rhum_09h_mean, file = "data/climate/musa_trials_rhum_09h_mean.csv")

as.numeric(musa_trials_rhum_09h[[1]])

#12h
musa_trials_rhum_12h <- ag5_extract(coords = musa_trials_time_loc,
                                    start_date = "pdate",
                                    variable = "Relative-Humidity-2m",
                                    time = "12h",
                                    path = "F:/env/AgERA5/rhum/",
                                    ncores = 4)


save(musa_trials_rhum_12h, file = "data/climate/musa_trials_rhum_12h.rda")


musa_trials_rhum_12h_mean <- lapply(musa_trials_rhum_12h, function(X) mean(as.numeric(X)))

musa_trials_rhum_12h_mean <- data.frame("rhum_12" = unlist(musa_trials_rhum_12h_mean))

#write.csv(musa_trials_rhum_12h_mean, file = "data/climate/musa_trials_rhum_12h_mean.csv")


#15h

musa_trials_rhum_15h <- ag5_extract(coords = musa_trials_time_loc,
                                   start_date = "pdate",
                                   variable = "Relative-Humidity-2m",
                                   time = "15h",
                                   path = "F:/env/AgERA5/rhum/",
                                   ncores = 4)


save(musa_trials_rhum_15h, file = "data/climate/musa_trials_rhum_15h.rda")


musa_trials_rhum_15h_mean <- lapply(musa_trials_rhum_15h, function(X) mean(as.numeric(X)))

musa_trials_rhum_15h_mean <- data.frame("rhum_15" = unlist(musa_trials_rhum_15h_mean))

#write.csv(musa_trials_rhum_15h_mean, file = "data/climate/musa_trials_rhum_15h_mean.csv")

#--------------------------------------------------------------
#18h

musa_trials_rhum_18h <- ag5_extract(coords = musa_trials_time_loc,
                                    start_date = "pdate",
                                    variable = "Relative-Humidity-2m",
                                    time = "18h",
                                    path = "F:/env/AgERA5/rhum/",
                                    ncores = 6)


save(musa_trials_rhum_18h, file = "data/climate/musa_trials_rhum_18h.rda")


musa_trials_rhum_18h_mean <- lapply(musa_trials_rhum_18h, function(X) mean(as.numeric(X)))

musa_trials_rhum_18h_mean <- data.frame("rhum_18" = unlist(musa_trials_rhum_18h_mean))

#write.csv(musa_trials_rhum_18h_mean, file = "data/climate/musa_trials_rhum_18h_mean.csv")

#--------------------------------------------------------------

#add rhum to previous climate data
musa_climate_data <- read.csv("data/processed/musa_trials_climate.csv")

musa_climate_data <- cbind(musa_climate_data,
                           musa_trials_rhum_06h_mean,
                           musa_trials_rhum_09h_mean,
                           musa_trials_rhum_12h_mean,
                           musa_trials_rhum_15h_mean,
                           musa_trials_rhum_18h_mean
                           )

write.csv(musa_climate_data, file = "data/processed/musa_trials_climate.csv", row.names = FALSE)





