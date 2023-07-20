#Check how many rankings were generated from each trial dataset

source("scripts/utils/load_data.R")
musa_trials_data

nrow(musa_trials_data)

x <- rownames(musa_trials_data)

grep(pattern = "imtp_1", x = x, value = T)

grep(pattern = "imtp_2", x = x, value = T)

sum(grepl(pattern = "imtp_2", x = x))

grep(pattern = "imtp_161", x = x, value = T)

sum(grepl(pattern = "imtp_161", x = x))

grep(pattern = "Ka|Mb|Mt|Ma|Ta", x = x, value = T)

sum(grepl(pattern = "Ka|Mb|Mt|Ma|Ta", x = x))

4 + 13 + 26 + 3 + 2 + 2 + 60

