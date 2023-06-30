#Climate information for the trial locations included in the paper

#Data from:
#"Essential climate variables for assessment of climate variability from 1979 to present"
#Hersbach, H., Muñoz Sabater, J., Nicolas, Rozum, I., Simmons, Vamborg, F., A., Bell, B., Berrisford, P., Biavati,
#G., Buontempo, C., Horányi, A., J., Peubey, C., Radu, R., Schepers, D., Soci, C., Dee, D., Thépaut, J-N. (2018):
#Essential climate variables for assessment of climate variability from 1979 to present. 
#Copernicus Climate Change Service (C3S) Data Store (CDS). (Accessed on 30-07-2023).

#Data downloaded with script: 
#"scripts/misc/download_ecv_climatologies_1990_2020.py"

library(terra)
library(sf)

musa_trials_loc <- read.csv("data/processed/trial_info_ext.csv", 
                            check.names = FALSE)

musa_trials_loc_sf <- sf::st_as_sf(musa_trials_loc,
                                   coords = c("lon", "lat"),
                                   crs = 4326)

musa_trials_loc_sf$trial <- row.names(musa_trials_loc_sf)

unq_locs_sf <- musa_trials_loc_sf[!duplicated(musa_trials_loc$lon), ]

unq_locs_sf <- unq_locs_sf[!duplicated(unq_locs_sf$location_name), ]

nrow(unq_locs_sf)

unq_locs_sf$x <- st_coordinates(unq_locs_sf)[,1]

unq_locs_sf$y <- st_coordinates(unq_locs_sf)[,2]

unq_locs_sf[unq_locs_sf$location_name == "Dole", ]$location_name <- "Kidapawan"

unq_locs_sf$id_2 <- seq(1:nrow(unq_locs_sf))

unq_locs_sf


#Temperture
temp_files <- list.files("data/climate/ecv_ERA5/climatology_1990_2020_ERA5/temp/",
                         full.names = TRUE)

temp_stack <- terra::rast(temp_files)

plot(temp_stack[[1]])

#Convert from kelvin to degree Celsius
temp_stack <- temp_stack - 273.15

dim(temp_stack)

#extract temperature values for trial locations
temp_trials <- terra::extract(temp_stack, terra::vect(unq_locs_sf),
                              list = F)

colnames(temp_trials)[grepl(pattern = "SFC",
                            colnames(temp_trials))] <- month.abb

unq_locs_sf$temp <- rowMeans(temp_trials[-1])

unq_locs_sf

#View(unq_locs_sf[, c("country", "location_name", "temp")])

##### Precipitation ##### 

prec_files <- list.files("data/climate/ecv_ERA5/climatology_1990_2020_ERA5/prec/", 
                         full.names = TRUE)

prec_stack <- terra::rast(prec_files)

#precipitation values are provided as m per day
#https://confluence.ecmwf.int/x/-ijnDg
#Convert to mm per month
prec_stack <- prec_stack * 1000 * 30

dim(prec_stack)

plot(prec_stack[[1]])

#prec_stack <- terra::rotate(prec_stack)

prec_trials <- terra::extract(prec_stack, terra::vect(unq_locs_sf), list = F)

colnames(prec_trials)[grepl(pattern = "SFC", colnames(prec_trials))] <- month.abb#c("id", month.abb)

#Get total avg year precipiation 
unq_locs_sf$prec <- rowSums(prec_trials[-1])

# prec_trials$trial_id <- unq_locs_sf$id

#View(unq_locs_sf[, c("country", "location_name", "temp", "prec")])

# prec_trials <- merge(x = unq_locs_sf, y = prec_trials, by.x = "id", by.y = "trial_id", order = F)
# 
# colnames(prec_trials)
# 
# prec_trials <- sf::st_drop_geometry(prec_trials)
# 
# prec_trials_df <- as.data.frame(prec_trials)
# 
# sel_cols <- c("id", "country", "location_name", "study", month.abb)
# 
# prec_trials_df <- prec_trials_df[sel_cols]
# 
# 
# which(duplicated(prec_trials_df$location_name))
# 
# prec_yr_avg <- rowMeans(prec_trials_df[, month.abb])
# 
# data.frame(prec_trials[, c("id", "country", "location_name", "study")],
#       "prec" = prec_yr_avg)



#rhum

rhum_files <- list.files("data/climate/ecv_ERA5/climatology_1990_2020_ERA5/rhum/", full.names = TRUE)

rhum_stack <- terra::rast(rhum_files)

rhum_stack <- rhum_stack

dim(rhum_stack)

rhum_trials <- terra::extract(rhum_stack, terra::vect(unq_locs_sf), list = F)

colnames(rhum_trials)[grepl(pattern = "SFC", colnames(rhum_trials))] <- month.abb#c("id", month.abb)

unq_locs_sf$rhum <- rowMeans(rhum_trials[-1])

unq_locs_sf

unq_locs_sf <- dplyr::arrange(unq_locs_sf, country)

table_2 <- sf::st_drop_geometry(unq_locs_sf)

table_2 <- table_2[, c("country", 
                           "location_name",
                           "x",
                           "y",
                           "temp",
                           "prec",
                           "rhum")]

table_2

write.csv(x = table_2,
          file = "output/table_2.csv")

write.csv(st_drop_geometry(unq_locs_sf), file = "data/processed/climatology_trials_1990_2020_ERA5.csv")

#############################################################



