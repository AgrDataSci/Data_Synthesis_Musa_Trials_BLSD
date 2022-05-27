#load all data
lapply(list.files("scripts/", pattern = "_prep", full.names = TRUE, recursive = TRUE),
       source)

id_loc_cols <- c("id_2",
                 "lat",
                 "lon",
                 "pdate",
                 "end_date")

musa_trials_data <- data.matrix(dplyr::bind_rows(imtp_1_rank_mtx[!colnames(imtp_1_rank_mtx)%in% id_loc_cols],
                                                 imtp_2_rank_mtx[!colnames(imtp_2_rank_mtx)%in% id_loc_cols],
                                                 imtp_3_rank_mtx[!colnames(imtp_3_rank_mtx)%in% id_loc_cols],
                                                 trial_16181_rank_mtx[!colnames(trial_16181_rank_mtx)%in% id_loc_cols],
                                                 narvaez_rank_mtx[!colnames(narvaez_rank_mtx)%in% id_loc_cols],
                                                 mgis_ranks_mtx[!colnames(mgis_ranks_mtx)%in% id_loc_cols],
                                                 bbb_ranks_1_3_4_mtx[!colnames(bbb_ranks_1_3_4_mtx)%in% id_loc_cols],
                                                 bbb_ranks_2_5_mtx[!colnames(bbb_ranks_2_5_mtx)%in% id_loc_cols]
                                                 ))

musa_trials_data[is.na(musa_trials_data)] <- 0


write.csv(musa_trials_data, file = "data/processed/musa_trials_data.csv", row.names = TRUE)

time_loc_cols <- c("lat",
                   "lon",
                   "pdate",
                   "end_date")


musa_trials_time_loc <- rbind(imtp_1_rank_mtx[time_loc_cols],
                              imtp_2_rank_mtx[time_loc_cols],
                              imtp_3_rank_mtx[time_loc_cols],
                              trial_16181_rank_mtx[time_loc_cols],
                              narvaez_rank_mtx[time_loc_cols],
                              mgis_ranks_mtx[time_loc_cols],
                              bbb_ranks_1_3_4_mtx[time_loc_cols],
                              bbb_ranks_2_5_mtx[time_loc_cols])


musa_trials_time_loc$eval_time <- as.numeric(as.Date(musa_trials_time_loc$end_date) - as.Date(musa_trials_time_loc$pdate))


musa_trials_time_loc$eval_time_months <- round(musa_trials_time_loc$eval_time / 30, 0)


musa_trials_time_loc



write.csv(musa_trials_time_loc, file = "data/processed/musa_trials_time_loc.csv", row.names = TRUE)



#location info for maps


trial_info <- musa_trials_time_loc

trial_info$id <- row.names(trial_info)

trial_info

trial_info <- trial_info[, c("id",
                             "lat",
                             "lon",
                             "pdate",
                             "end_date")]

trial_info <- trial_info[!grepl(pattern = "(c2)$", trial_info$id), ]

trial_info <- trial_info[!grepl(pattern = "(ratoon)$", trial_info$id), ]

trial_info <- trial_info[!grepl(pattern = "_[B-Z]$", trial_info$id,), ]

trial_info <- trial_info[!grepl(pattern = "_[2-4]$", trial_info$id), ]

trial_info <- trial_info[!grepl(pattern = "_rt", trial_info$id), ]

trial_info <- trial_info[!grepl(pattern = "alley", trial_info$id), ]

trial_info

trial_info$id_2 <- rep("", nrow(trial_info))

nrow(trial_info)

write.csv(trial_info, file = "data/processed/trial_info.csv", row.names = FALSE)





