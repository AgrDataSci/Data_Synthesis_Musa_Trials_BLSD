library(readr)
library(janitor)
library(gosset)
library(PlackettLuce)

library(dplyr)

source(file = "scripts/utils/correct_musa_names.R")

bbb_locations <- read_csv("data/musabase/bbb_locations_pdates.csv")

bbb_files <- list.files("data/musabase/bbb_project_trials/", pattern = "\\.csv", full.names = TRUE)

bbb_data <- lapply(bbb_files, function(X) read_csv(X))

bbb_data <- lapply(bbb_data, janitor::clean_names)

for(i in seq_along(bbb_data)){
  
  colnames(bbb_data[[i]])[7] <- "genotypes"
  
}

colnames(bbb_data[[1]])

grep(pattern = "shooting", x = colnames(bbb_data[[1]]))

bbb_data <- lapply(bbb_data, correct_names)

bbb_data_cols_sel <- c("number_k",
                       "number_h",
                       "country_id",
                       "envnt",
                       "env_id",
                       "plot_name_y",
                       "genotypes",
                       "plot_number_y",
                       "plot_id",
                       "block_number",
                       "is_a_control",
                       "rep_number",
                       "row_number",
                       "col_number",
                       "number_g",
                       "location_name_y",
                       "which_cycle_y",
                       "block_id_y",
                       "plot_id_y",
                       "black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean",
                       "harvest_leafspot_number_standing_leaves_co_325_0000759_mean",
                       "harvest_youngest_leaf_with_bls_symptoms_mean",
                       "days_planting_to_shooting_mean",
                       "days_planting_to_harvest_mean",
                       "previous_cycle_days_planting_to_harvest_mean",
                       "cycle_length"
                        )

bbb_data <- lapply(bbb_data, function(X) X[bbb_data_cols_sel])

for(i in seq_along(bbb_data)){
  
  bbb_data[[i]]$black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean <- as.numeric(bbb_data[[i]]$black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean)
  bbb_data[[i]]$harvest_youngest_leaf_with_bls_symptoms_mean <- as.numeric(bbb_data[[i]]$harvest_youngest_leaf_with_bls_symptoms_mean)
  bbb_data[[i]]$harvest_leafspot_number_standing_leaves_co_325_0000759_mean <- as.numeric(bbb_data[[i]]$harvest_leafspot_number_standing_leaves_co_325_0000759_mean)
  bbb_data[[i]]$days_planting_to_harvest_mean <- as.numeric(bbb_data[[i]]$days_planting_to_harvest_mean)
  bbb_data[[i]]$cycle_length <- as.numeric(bbb_data[[i]]$cycle_length)
  bbb_data[[i]]$previous_cycle_days_planting_to_harvest_mean <- as.numeric(bbb_data[[i]]$previous_cycle_days_planting_to_harvest_mean)
  
}

bbb_data <- lapply(bbb_data, function(X) X[!is.na(X$black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean),])

lapply(bbb_data, function(X) X[!is.na(X$harvest_youngest_leaf_with_bls_symptoms_mean),])

#exploratory analysis to decide which variable to use
lapply(bbb_data, function(X){
  
  #paste("black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean", var(X$black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean))
  #paste("harvest_youngest_leaf_with_bls_symptoms_mean", var(X$harvest_youngest_leaf_with_bls_symptoms_mean))
  paste("harvest_youngest_leaf_with_bls_symptoms_mean", var(X$harvest_leafspot_number_standing_leaves_co_325_0000759_mean))
  
})


#trials 1-3 should be handled separately from trial 4
#because in 4 black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean
#was not available, therefore  harvest_leafspot_number_standing_leaves_co_325_0000759_mean
#was used

bbb_data_1_3_4 <- do.call(rbind, bbb_data[c(1,3,4)])

bbb_data_1_3_4$plot_id
bbb_data_1_3_4$plot_id_y
bbb_data_1_3_4$plot_name_y

#View(bbb_data_1_3_4)

bbb_data_1_3_4 <- as.data.frame(bbb_data_1_3_4)

bbb_data_1_3_4

unique(bbb_data_1_3_4$which_cycle_y)

head(bbb_data_1_3_4[bbb_data_1_3_4$which_cycle_y == "firstplant", 
                  c("plot_id", 
                    "genotypes",
                    "days_planting_to_harvest_mean",
                    "black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean")])

head(bbb_data_1_3_4[bbb_data_1_3_4$which_cycle_y == "ratoon1",
                  c("plot_id", 
                    "genotypes",
                    "days_planting_to_harvest_mean",
                    "black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean")])

head(bbb_data_1_3_4[bbb_data_1_3_4$which_cycle_y == "ratoon2", 
                  c("plot_id", 
                    "genotypes",
                    "days_planting_to_harvest_mean",
                    "black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean")])


length(bbb_data_1_3_4[bbb_data_1_3_4$which_cycle_y == "ratoon1",
             "days_planting_to_harvest_mean"]) - length(bbb_data_1_3_4[bbb_data_1_3_4$which_cycle_y == "firstplant",
                                                             "days_planting_to_harvest_mean"])



bbb_data_1_3_4$id_2 <- paste(bbb_data_1_3_4$envnt, bbb_data_1_3_4$block_number, sep = "_")


bbb_data_1_3_4_agg <- aggregate(black_sigatoka_disease_infection_index_at_harvesting_comp_0000281_mean ~ id_2 + genotypes,
                              data = bbb_data_1_3_4,
                              FUN = mean)

bbb_data_1_3_4_time <- aggregate(days_planting_to_harvest_mean ~ id_2 ,
                              data = bbb_data_1_3_4,
                              FUN = mean)

bbb_locations

unique(bbb_data_1_3_4_time$id_2)

bbb_data_1_3_4_time$id <- ifelse(grepl(pattern = "Mb", x = bbb_data_1_3_4_time$id_2),"Mbarara", 
                               ifelse((grepl(pattern = "Mt", x = bbb_data_1_3_4_time$id_2)), "Mitalula", "Kawanda"))





bbb_data_1_3_4_time_loc <- merge(x = bbb_data_1_3_4_time,
                               y = bbb_locations,
                               by.x = "id",
                               by.y = "Site")


bbb_data_1_3_4_time_loc <-  bbb_data_1_3_4_time_loc[, c("id_2",
                                                    "days_planting_to_harvest_mean",
                                                    "Latitude",
                                                    "Longitude",
                                                    "pdate")]

bbb_data_1_3_4_time_loc$pdate <- as.Date(bbb_data_1_3_4_time_loc$pdate, format = "%m/%d/%Y")


bbb_data_1_3_4_time_loc$end_date <- bbb_data_1_3_4_time_loc$pdate + bbb_data_1_3_4_time_loc$days_planting_to_harvest_mean

bbb_data_1_3_4_time_loc <- bbb_data_1_3_4_time_loc[, c("id_2",
                                                   "Latitude",
                                                   "Longitude",
                                                   "pdate",
                                                   "end_date")]

names(bbb_data_1_3_4_time_loc) <- c("id_2",
                                  "lat",
                                  "lon",
                                  "pdate",
                                  "end_date")


bbb_ranks_1_3_4 <- gosset::rank_numeric(data = bbb_data_1_3_4_agg,
                                      items = 2,
                                      input = 3,
                                      id = 1,
                                      ascending = TRUE)

bbb_ranks_1_3_4_mtx <- as.data.frame(unclass(bbb_ranks_1_3_4))

#row.names(bbb_ranks_1_3_4_mtx) <- unique(bbb_data_1_3_4_agg$id_2)

bbb_ranks_1_3_4_mtx <- cbind(bbb_ranks_1_3_4_mtx, bbb_data_1_3_4_time_loc)

row.names(bbb_ranks_1_3_4_mtx) <- bbb_ranks_1_3_4_mtx$id_2

bbb_ranks_1_3_4_mtx

bbb_ranks_1_3_4_mtx[c("pdate", "end_date")]

#Correct start date for second and third cycles

trial_prefix_list <- gsub(pattern = "_1|_2|_3|_4|_fp|_rt1|_rt2", replacement = "", x = bbb_ranks_1_3_4_mtx$id_2)

trial_prefix_list <- unique(trial_prefix_list)

cycle_prefix_list <- c("_fp", "_rt1", "_rt2")

for(i in seq_along(trial_prefix_list)){
  for(j in 2:length(cycle_prefix_list)){
    cycle_2_trials <- paste0(trial_prefix_list[i], cycle_prefix_list[j])
    
    cycle_1_trials <- paste0(trial_prefix_list[i], cycle_prefix_list[j-1])
    
    bbb_ranks_1_3_4_mtx[grepl(cycle_2_trials, 
                            bbb_ranks_1_3_4_mtx$id_2), ]$pdate <- bbb_ranks_1_3_4_mtx[grepl(cycle_1_trials,
                                                                                        bbb_ranks_1_3_4_mtx$id_2), ]$end_date
    
  }
}

bbb_ranks_1_3_4_mtx[grepl("Ka", bbb_ranks_1_3_4_mtx$id_2), c("pdate", "end_date")]
bbb_ranks_1_3_4_mtx[grepl("Mb", bbb_ranks_1_3_4_mtx$id_2), c("pdate", "end_date")]
bbb_ranks_1_3_4_mtx[grepl("Mt", bbb_ranks_1_3_4_mtx$id_2), c("pdate", "end_date")]

bbb_ranks_1_3_4_mtx <- bbb_ranks_1_3_4_mtx[!colnames(bbb_ranks_1_3_4_mtx) %in% "id_2"]

# bbb_ranks_1_3_4_mtx$ev_time <- bbb_ranks_1_3_4_mtx$end_date - bbb_ranks_1_3_4_mtx$pdate
# 
# bbb_ranks_1_3_4_mtx$ev_time / 30 


#### Trials 2 and 5 BBB project ####

#bbb_data_2_5 <- bbb_data[[4]]

bbb_data_2_5 <- do.call(rbind, bbb_data[c(2,5)])

bbb_data_2_5$harvest_leafspot_number_standing_leaves_co_325_0000759_mean <- as.numeric(bbb_data_2_5$harvest_leafspot_number_standing_leaves_co_325_0000759_mean)

bbb_data_2_5 <- bbb_data_2_5[!is.na(bbb_data_2_5$harvest_leafspot_number_standing_leaves_co_325_0000759_mean), ]

bbb_data_2_5$days_planting_to_harvest_mean <- as.numeric(bbb_data_2_5$days_planting_to_harvest_mean)


bbb_data_2_5$id_2 <- paste(bbb_data_2_5$envnt, bbb_data_2_5$block_number, sep = "_")

bbb_data_2_5_agg <- aggregate(harvest_leafspot_number_standing_leaves_co_325_0000759_mean ~ id_2 + genotypes,
                            data = bbb_data_2_5,
                            FUN = mean)

#remove evaluation with only one genotype

to_remv_2_5 <- names(which(rowSums(table(bbb_data_2_5_agg)) <= 1))


bbb_data_2_5_agg <- bbb_data_2_5_agg[bbb_data_2_5_agg$id_2 != to_remv_2_5, ]


bbb_data_2_5_time <- aggregate(days_planting_to_harvest_mean ~ id_2,
                             data = bbb_data_2_5,
                             FUN = mean)


bbb_data_2_5_time$id <- ifelse(grepl(pattern = "Ma", x = bbb_data_2_5_time$id_2),"Maruku", 
                               "TaCRI")

bbb_data_2_5_time_loc <- merge(x = bbb_data_2_5_time,
                                 y = bbb_locations,
                                 by.x = "id",
                                 by.y = "Site")

bbb_data_2_5_time_loc <-  bbb_data_2_5_time_loc[, c("id_2",
                                                    "days_planting_to_harvest_mean",
                                                    "Latitude",
                                                    "Longitude",
                                                    "pdate")]

bbb_data_2_5_time_loc$pdate <- as.Date(bbb_data_2_5_time_loc$pdate, format = "%m/%d/%Y")


bbb_data_2_5_time_loc$end_date <- bbb_data_2_5_time_loc$pdate + bbb_data_2_5_time_loc$days_planting_to_harvest_mean

bbb_data_2_5_time_loc <- bbb_data_2_5_time_loc[, c("id_2",
                                                   "Latitude",
                                                   "Longitude",
                                                   "pdate",
                                                   "end_date")]

names(bbb_data_2_5_time_loc) <- c("id_2",
                                    "lat",
                                    "lon",
                                    "pdate",
                                    "end_date")


bbb_data_2_5_time_loc <- bbb_data_2_5_time_loc[bbb_data_2_5_time_loc$id_2 %in% bbb_data_2_5_agg$id_2, ]

#bbb_data_2_5_time_loc <- bbb_locations[bbb_locations$Site == "TaCRI", c("Latitude","Longitude","pdate")]

#bbb_data_2_5_time_loc <- cbind(bbb_data_2_5_time, bbb_data_2_5_time_loc)

# bbb_data_2_5_time_loc$pdate <- as.Date(bbb_data_2_5_time_loc$pdate, format = "%m/%d/%Y")
# 
# bbb_data_2_5_time_loc$end_date <- bbb_data_2_5_time_loc$pdate + bbb_data_2_5_time_loc$days_planting_to_harvest_mean
# 
# 
# bbb_data_2_5_time_loc <- bbb_data_2_5_time_loc[, c("id_2", "Latitude", "Longitude", "pdate", "end_date")]
# 
# 
# names(bbb_data_2_5_time_loc) <- c("id_2",
#                                 "lat",
#                                 "lon",
#                                 "pdate",
#                                 "end_date")

bbb_ranks_2_5 <- gosset::rank_numeric(data = bbb_data_2_5_agg,
                                      items = 2,
                                      input = 3,
                                      id = 1,
                                      ascending = FALSE)


bbb_ranks_2_5_mtx <- as.data.frame(unclass(bbb_ranks_2_5))


#row.names(bbb_ranks_2_5_mtx) <- unique(bbb_data_2_5_agg$id_2)

bbb_ranks_2_5_mtx <- cbind(bbb_ranks_2_5_mtx, bbb_data_2_5_time_loc)

row.names(bbb_ranks_2_5_mtx) <- bbb_ranks_2_5_mtx$id_2

bbb_ranks_2_5_mtx

#Correct start date for ratoon cycles

bbb_ranks_2_5_mtx[grepl("Ma_rt1", bbb_ranks_2_5_mtx$id_2), ]$pdate <- bbb_ranks_2_5_mtx[grepl("Ma_fp", bbb_ranks_2_5_mtx$id_2), ]$end_date


bbb_ranks_2_5_mtx[grepl("Ma_rt2", bbb_ranks_2_5_mtx$id_2), ]$pdate <- bbb_ranks_2_5_mtx[grepl(c("Ma_rt1_1|Ma_rt1_2|Ma_rt1_4"), 
                                                                                              bbb_ranks_2_5_mtx$id_2), ]$end_date


bbb_ranks_2_5_mtx[grepl("Ma_rt3", bbb_ranks_2_5_mtx$id_2), ]$pdate <- bbb_ranks_2_5_mtx[grepl(c("Ma_rt2_1|Ma_rt2_2|Ma_rt2_4"), 
                                                                                              bbb_ranks_2_5_mtx$id_2), ]$end_date

bbb_ranks_2_5_mtx[grepl("Ma_rt4", bbb_ranks_2_5_mtx$id_2), ]$pdate <- bbb_ranks_2_5_mtx[grepl("Ma_rt3_4", 
                                                                                              bbb_ranks_2_5_mtx$id_2), ]$end_date




# #cycle_prefix_list <- c("_fp", "_rt1", "_rt2")
# 
for(j in 2:length(cycle_prefix_list)){

  cycle_2_trials <- paste0("Ta", cycle_prefix_list[j])

  cycle_1_trials <- paste0("Ta", cycle_prefix_list[j - 1])

  bbb_ranks_2_5_mtx[grepl(cycle_2_trials,
                          bbb_ranks_2_5_mtx$id_2),
                  ]$pdate <- bbb_ranks_2_5_mtx[grepl(cycle_1_trials,
                                                   bbb_ranks_2_5_mtx$id_2),
                  ]$end_date
}

bbb_ranks_2_5_mtx[grepl("Ma", bbb_ranks_2_5_mtx$id_2), c("pdate", "end_date")]
bbb_ranks_2_5_mtx[grepl("Ta", bbb_ranks_2_5_mtx$id_2), c("pdate", "end_date")]


bbb_ranks_2_5_mtx <- bbb_ranks_2_5_mtx[!colnames(bbb_ranks_2_5_mtx) %in% "id_2"]


bbb_ranks_2_5_mtx$ev_time <- bbb_ranks_2_5_mtx$end_date - bbb_ranks_2_5_mtx$pdate


bbb_ranks_2_5_mtx <- bbb_ranks_2_5_mtx[bbb_ranks_2_5_mtx$ev_time > 60, ]


bbb_ranks_2_5_mtx <- bbb_ranks_2_5_mtx[, !colnames(bbb_ranks_2_5_mtx) %in% "ev_time"]


