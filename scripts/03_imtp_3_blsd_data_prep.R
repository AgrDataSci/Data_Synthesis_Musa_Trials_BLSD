library(readr)
library(janitor)
library(PlackettLuce)
library(gosset)

source("scripts/utils/correct_musa_names.R")

#imtp_3_folder <- "data/agtrials_banana/clean/imtp/imtp_3/black_sigatoka/trial_data_csv/"

imtp_3_folder <- "data/agtrials/imtp/imtp_3/blsd"

#imtp_3_files <- list.files(path = imtp_3_folder, pattern = "\\.csv", full.names = TRUE)

imtp_3_files <- list.files(path = imtp_3_folder, pattern = "\\.csv", full.names = TRUE, recursive = TRUE) 

imtp_3_data <- lapply(imtp_3_files[1:6], read_csv)

imtp_3_data

imtp_3_data <- lapply(imtp_3_data, janitor::clean_names)

lapply(imtp_3_data, colnames)

#Use file names to identify each trial
trial_names_imtp3 <- gsub(pattern = "data/agtrials/imtp/imtp_3/blsd/TrialData[0-9]+/|_trial_data|\\.csv|-3",
                          replacement = "",
                          x = imtp_3_files[1:6])

names(imtp_3_data) <- trial_names_imtp3

imtp_3_data

length(imtp_3_data)

lapply(imtp_3_data, colnames)

cols_to_search_imtp_3 <- c("cultivar",
                           "block", 
                           "plant",
                           "mat_id",
                           "plant_cycle",
                           "date_planted",
                           "date_shooting",
                           "youngest_leaf_spotted_shooting",
                           "disease_development_time_days_growing_phase",
                           "youngest_leaf_spotted_growing_phase",
                           "number_of_functional_leaves_shooting")

imtp_3_data <- lapply(imtp_3_data, function(X) X[, names(X) %in% cols_to_search_imtp_3])

for(i in 1:length(imtp_3_data)){
  
 names(imtp_3_data[[i]])[1] <- "genotypes"
  
}

imtp_3_data <- lapply(imtp_3_data, correct_names)

imtp_3_data <- lapply(imtp_3_data, as.data.frame)

lapply(imtp_3_data, function(X) unique(X$genotypes))

#There is no information about what is "Local" variety in trial 16166, then it should be removed
imtp_3_data$imtp_16166 <- imtp_3_data$imtp_16166[!imtp_3_data$imtp_16166$genotypes == "Local", ]

lapply(imtp_3_data, head)

lapply(imtp_3_data, nrow)

lapply(imtp_3_data, colnames)

#change youngest_leaf_spotted_growing_phase to youngest_leaf_spotted_shooting in trial 16170
imtp_16170_yls_idx <- which(colnames(imtp_3_data$imtp_16170) == "youngest_leaf_spotted_growing_phase")
colnames(imtp_3_data$imtp_16170)[imtp_16170_yls_idx] <- "youngest_leaf_spotted_shooting"

n_yls <- unlist(lapply(imtp_3_data, function(X) nrow(X[!is.na(X$youngest_leaf_spotted),])))

n_ddt <- unlist(lapply(imtp_3_data, function(X) nrow(X[!is.na(X$disease_development_time_days_growing_phase),])))

n_nfl <- unlist(lapply(imtp_3_data, function(X) nrow(X[!is.na(X$number_of_functional_leaves_shooting),])))

data.frame("trial" = names(imtp_3_data),
           n_yls,
           n_ddt,
           n_nfl)

#trials "imtp_16166" "imtp_16167" "imtp_16170" "imtp_16176" - Use YLS

imtp_3_data$imtp_16166$bls_rating <- imtp_3_data$imtp_16166$youngest_leaf_spotted_shooting

imtp_3_data$imtp_16167$bls_rating <- imtp_3_data$imtp_16167$youngest_leaf_spotted_shooting

imtp_3_data$imtp_16170$bls_rating <- imtp_3_data$imtp_16170$youngest_leaf_spotted_shooting

imtp_3_data$imtp_16176$bls_rating <- imtp_3_data$imtp_16176$number_of_functional_leaves_shooting

#trials  "imtp_16177" "imtp_16178" use Number of leaf at shooting


imtp_3_data$imtp_16177$bls_rating <- imtp_3_data$imtp_16177$number_of_functional_leaves_shooting

imtp_3_data$imtp_16178$bls_rating <- imtp_3_data$imtp_16178$number_of_functional_leaves_shooting



#Select required columns
imtp_3_data <- lapply(imtp_3_data, function(X) X[, colnames(X) %in% c("genotypes",
                                                                      "block", 
                                                                      "plant",
                                                                      "plant_cycle",
                                                                      "date_planted",
                                                                      "date_shooting",
                                                                      "bls_rating")])

imtp_3_data <- lapply(imtp_3_data, function(X) X[!is.na(X$bls_rating), ])

sum(unlist(lapply(imtp_3_data, nrow)))

lapply(lapply(imtp_3_data, function(X) X[X$plant_cycle == "mother",]), nrow)

lapply(lapply(imtp_3_data, function(X) X[X$plant_cycle == "ratoon",]), nrow)

lapply(imtp_3_data, function(X) X[X$plant_cycle == "ratoon",]$date_planted)

lapply(imtp_3_data, function(X) X[X$plant_cycle == "ratoon",]$date_shooting)

unlist(lapply(imtp_3_data, nrow))

unlist(lapply(lapply(imtp_3_data, function(X) X[X$plant_cycle == "mother",]), nrow)) +

unlist(lapply(lapply(imtp_3_data, function(X) X[X$plant_cycle == "ratoon",]), nrow))

imtp_3_data$imtp_16177

for(i in seq_along(imtp_3_data)){
  
  imtp_3_data[[i]]$id <- names(imtp_3_data[i])
  
}

#trial 16166 has some data points with dates with problematic format
imtp_3_data$imtp_16166$date_planted <- gsub(pattern = "/02", 
                                             x = imtp_3_data$imtp_16166$date_planted,
                                             replacement = "/2002")

imtp_3_data$imtp_16166$date_shooting <- gsub(pattern = "/03", 
                                             x = imtp_3_data$imtp_16166$date_shooting,
                                             replacement = "/2003")

imtp_3_data$imtp_16166$date_shooting <- gsub(pattern = "/02", 
                                             x = imtp_3_data$imtp_16166$date_shooting,
                                             replacement = "/2002")

#now format the dates of the rest of the data



for(i in seq_along(imtp_3_data)){
  
  if(names(imtp_3_data[i]) == "imtp_16166"){
    imtp_3_data[[i]]$date_planted <- as.Date(imtp_3_data[[i]]$date_planted,
                                            format = "%d/%m/%Y")
    
    imtp_3_data[[i]]$date_shooting <- as.Date(imtp_3_data[[i]]$date_shooting, 
                                              format = "%d/%m/%Y")
    
    mean_date_planted <- mean(imtp_3_data[[i]]$date_planted, 
                              na.rm = T)
    
    mean_date_shooting <- mean(imtp_3_data[[i]]$date_shooting, 
                               na.rm = T)
    
    no_date_planted <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted), 
                                               "date_planted"])
    
    no_date_shooting <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting), 
                                                "date_shooting"])
    
    #Replace NA's with mean values
    
    imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted), "date_planted"] <- rep(mean_date_planted,
                                                                                  no_date_planted)
    
    imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting), "date_shooting"] <- rep(mean_date_shooting,
                                                                                    no_date_shooting)
    
    
   }
  else{

  imtp_3_data[[i]]$date_planted <- as.Date(imtp_3_data[[i]]$date_planted,
                                           format = "%m/%d/%Y")

  imtp_3_data[[i]]$date_shooting <- as.Date(imtp_3_data[[i]]$date_shooting,
                                            format = "%m/%d/%Y")

  mean_date_planted <- mean(imtp_3_data[[i]]$date_planted,
                            na.rm = T)

  mean_date_shooting <- mean(imtp_3_data[[i]]$date_shooting,
                             na.rm = T)

  no_date_planted <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted),
                                             "date_planted"])

  no_date_shooting <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting),
                                              "date_shooting"])

  #Replace NA's with mean values

  imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted), "date_planted"] <- rep(mean_date_planted,
                                                                                no_date_planted)

  imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting), "date_shooting"] <- rep(mean_date_shooting,
                                                                                  no_date_shooting)
  }
  
}

imtp_3_data_df <- dplyr::bind_rows(imtp_3_data)

imtp_3_data_df[is.na(imtp_3_data_df$plant_cycle), ]$plant_cycle <- "mother"

unique(imtp_3_data_df$plant_cycle)

imtp_3_data_df$id_2 <- paste(imtp_3_data_df$id,
                           imtp_3_data_df$plant_cycle,
                           imtp_3_data_df$block,
                           sep = "_")

#ratoon plant cycle should be removed, as it doesn't have time
imtp_3_data_df <- imtp_3_data_df[imtp_3_data_df$plant_cycle == "mother", ]

imtp_3_data_agg <- aggregate(bls_rating ~ id_2 + genotypes,
                                data = imtp_3_data_df,
                                FUN = mean)

head(imtp_3_data_agg)

nrow(imtp_3_data_agg)


imtp_3_rank <- gosset::rank_numeric(data = imtp_3_data_agg,
                                    items = 2,
                                    input = 3,
                                    id = 1,
                                    group = FALSE,
                                    ascending = FALSE)


imtp_3_rank

imtp_3_data_pdate <- aggregate(date_planted ~ id_2 ,
                               data = imtp_3_data_df,
                               FUN = function(X) mean(X, na.rm = T))


imtp_3_data_sdate <- aggregate(date_shooting ~ id_2 ,
                               data = imtp_3_data_df,
                               FUN = function(X) mean(X, na.rm = T))

imtp_3_data_dates <- merge(x = imtp_3_data_pdate,
                           y = imtp_3_data_sdate,
                           by = "id_2")

imtp_3_rank_mtx <- as.data.frame(unclass(imtp_3_rank))

unique(imtp_3_data_df$id_2)

row.names(imtp_3_rank_mtx) <- unique(imtp_3_data_df$id_2)


imtp_3_rank_mtx$trial_id <- gsub(pattern = "_|[A-Z]|[a-z]",
                                 replacement = "",
                                 row.names(imtp_3_rank_mtx))

imtp_3_rank_mtx

nrow(imtp_3_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id2

imtp_3_loc_dates <- imtp_loc_dates[imtp_loc_dates$trial_id2 %in% imtp_3_rank_mtx$trial_id , ]

imtp_3_loc_dates <- imtp_3_loc_dates[imtp_3_loc_dates$cycle == "mother", ]

imtp_3_loc_dates <- imtp_3_loc_dates[c("trial_id2",
                                       "latitude", 
                                       "longitude")]

nrow(imtp_3_rank_mtx)

imtp_3_rank_mtx <- merge(x = imtp_3_rank_mtx,
                         y = imtp_3_loc_dates,
                         by.x = "trial_id",
                         by.y = "trial_id2"
                        )


row.names(imtp_3_rank_mtx) <- unique(imtp_3_data_df$id_2)

#imtp_3_rank_mtx$pdate <- as.Date(imtp_3_rank_mtx$pdate, format = "%m/%d/%Y")

imtp_3_rank_mtx <- imtp_3_rank_mtx[-1]

imtp_3_rank_mtx <- cbind(imtp_3_rank_mtx, imtp_3_data_dates[, c("date_planted", "date_shooting")])

imtp_3_rank_mtx


imtp_3_loc_dates_cols <- which(colnames(imtp_3_rank_mtx) %in% c("latitude",
                                                                "longitude",
                                                                "date_planted",
                                                                "date_shooting"))

names(imtp_3_rank_mtx)[imtp_3_loc_dates_cols] <-  c("lat","lon","pdate","end_date")

head(imtp_3_rank_mtx)


#### trial 16181 is in a separate file/dataset ####

trial_16181_agr<-read_csv("data/agtrials/imtp/imtp_3/blsd/TrialData16181/imtp-3_16181_trial_data_agronomic.csv")

trial_16181_di<-read_csv("data/agtrials/imtp/imtp_3/blsd/TrialData16181/imtp-3_16181_trial_data_disease.csv")

trial_16181_agr <- janitor::clean_names(trial_16181_agr)
trial_16181_di  <- janitor::clean_names(trial_16181_di)

head(trial_16181_agr)
head(trial_16181_di)

trial_16181_agr <- as.data.frame(trial_16181_agr)

trial_16181_di <- as.data.frame(trial_16181_di)

nrow(trial_16181_agr)

nrow(trial_16181_di)

#merge disease and agronomic datasets

trial_16181_data <- merge(x = trial_16181_agr,
                          y = trial_16181_di,
                          by.x = c("cultivar", "plant", "mat_id", "plant_cycle"),
                          by.y = c("cultivar", "plant", "mat_id", "plant_cycle"))

colnames(trial_16181_data)[which(colnames(trial_16181_data) == "cultivar")] <- "genotypes"


trial_16181_data <- correct_names(trial_16181_data)

colnames(trial_16181_data)
#check which variable has the higher number of NA's
sum(is.na(trial_16181_data$number_of_functional_leaves_shooting))
sum(is.na(trial_16181_data$infection_index))

#check variance of each variable
var(trial_16181_data$number_of_functional_leaves_shooting[!is.na(trial_16181_data$number_of_functional_leaves_shooting)])
var(trial_16181_data$infection_index[!is.na(trial_16181_data$infection_index)])

hist(trial_16181_data$number_of_functional_leaves_shooting[!is.na(trial_16181_data$number_of_functional_leaves_shooting)])
hist(trial_16181_data$infection_index[!is.na(trial_16181_data$infection_index)])

#infection_index has higher number of NA's but is more continuous and has higher variability

#trial_16181_data <- trial_16181_data[!is.na(trial_16181_data$infection_index), ]

#I have selected number_of_functional_leaves_shooting because it indicates the moment (shooting)
#whereas in the case of 'infection index' we don't know if it was at shooting or at harvest.
#We only have day of shooting

trial_16181_data <- trial_16181_data[!is.na(trial_16181_data$number_of_functional_leaves_shooting), ]


trial_16181_data$id <- paste("imtp_16181",
                             trial_16181_data$plant_cycle,
                             sep = "_")




trial_16181_data_agg <- aggregate(number_of_functional_leaves_shooting ~ id + genotypes, data = trial_16181_data, FUN = mean)

trial_16181_rank <- gosset::rank_numeric(data = trial_16181_data_agg,
                                         items = 2,
                                         input = 3,
                                         id = 1,
                                         ascending = FALSE)


trial_16181_rank_mtx <- unclass(trial_16181_rank)

row.names(trial_16181_rank_mtx) <- unique(trial_16181_data_agg$id)

trial_16181_rank_mtx <- as.data.frame(trial_16181_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id

imtp_loc_dates$trial_id3 <- gsub(pattern = "-1|-2|-3|_trial_data", 
                                 replacement = "",
                                 x = imtp_loc_dates$trial_id)

trial_16181_loc <- imtp_loc_dates[imtp_loc_dates$trial_id2 == 16181 & imtp_loc_dates$cycle == "mother",
                                  c("latitude", "longitude", "planting_date")]

trial_16181_data$date_planted <- as.Date(trial_16181_data$date_planted, format = "%d/%m/%Y")

trial_16181_data$date_shooting <- as.Date(trial_16181_data$date_shooting, format = "%d/%m/%Y")


trial_16181_mother_pdate <- aggregate(date_planted ~ id, data = trial_16181_data, FUN = mean)

trial_16181_mother_sdate <- aggregate(date_shooting ~ id, data = trial_16181_data, FUN = mean)

trial_16181_mother_dates <- merge(trial_16181_mother_pdate, trial_16181_mother_sdate)

trial_16181_data[!is.na(trial_16181_data$crop_cycle_days), "plant_cycle"]

ratoon_crop_cycle <- aggregate(crop_cycle_days  ~ id, 
                               data = trial_16181_data[!is.na(trial_16181_data$crop_cycle_days), ],
                               FUN = mean)


ratoon_crop_cycle

trial_16181_ratoon_pdate <- trial_16181_mother_sdate$date_shooting

trial_16181_ratoon_sdate <- trial_16181_ratoon_pdate + ratoon_crop_cycle$crop_cycle_days

trial_16181_ratoon_dates <- data.frame("id" = "imtp_16181_ratoon", 
                                     "date_planted" = trial_16181_ratoon_pdate,
                                     "date_shooting" = trial_16181_ratoon_sdate)


trial_16181_dates <- rbind(trial_16181_mother_dates,
                           trial_16181_ratoon_dates)


trial_16181_rank_mtx <- cbind(trial_16181_rank_mtx, trial_16181_loc[-3], trial_16181_dates[-1])

trial_16181_rank_mtx


trial_16181_loc_dates_cols <- which(colnames(trial_16181_rank_mtx) %in% c("latitude",
                                                                          "longitude",
                                                                          "date_planted",
                                                                          "date_shooting"))

names(trial_16181_rank_mtx)[trial_16181_loc_dates_cols] <-  c("lat","lon","pdate","end_date")


#### End of trial 16181 ####


