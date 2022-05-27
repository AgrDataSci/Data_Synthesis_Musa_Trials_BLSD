musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1)


musa_trials_time_loc

ceiling(as.numeric(as.Date(musa_trials_time_loc$end_date) - as.Date(musa_trials_time_loc$pdate)) / 365)
musa_trials_time_loc$eval_time <- as.numeric(as.Date(musa_trials_time_loc$end_date) - as.Date(musa_trials_time_loc$pdate))


musa_trials_time_loc$eval_time_months <- round(musa_trials_time_loc$eval_time / 30)


musa_trials_time_loc


imtp_2_rank_mtx$ev_time <- imtp_2_rank_mtx$end_date - imtp_2_rank_mtx$pdate 


bbb_ranks_1_3_4_mtx$end_date - bbb_ranks_1_3_4_mtx$pdate


bbb_ranks_2_5_mtx$ev_time <- bbb_ranks_2_5_mtx$end_date - bbb_ranks_2_5_mtx$pdate
bbb_ranks_2_5_mtx
