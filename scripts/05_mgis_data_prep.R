library(readr)
library(gosset)
library(PlackettLuce)
library(dplyr)


source("scripts/utils/correct_musa_names.R")

#mgis_6
#Irish, B. M., Goenaga, R., Rios, C., Chavarria-Carvajal, J., & Ploetz, R. (2013). 
#Evaluation of banana hybrids for tolerance to black leaf streak (Mycosphaerella fijiensis Morelet) in Puerto Rico.
#Crop Protection, 54, 229-238. https://doi.org/https://doi.org/10.1016/j.cropro.2013.09.003 

#mgis_20
#Irish, B. M., Goenaga, R., Montalvo-Katz, S., Chaves-Cordoba, B., & Van den Bergh, I. (2019).
#Host Response to Black Leaf Streak and Agronomic Performance of Banana Genotypes in Puerto Rico. 
#HortScience horts, 54(10), 1808. https://doi.org/10.21273/hortsci13876-19

mgis_6_c1 <- read_csv("data/mgis/mgis_6/mgis_6_c1.csv")
mgis_6_c2 <- read_csv("data/mgis/mgis_6/mgis_6_c2.csv")

mgis_6_c1$trial_r <- "mgis_6_c1"
mgis_6_c2$trial_r <- "mgis_6_c2"
mgis_6 <- rbind(mgis_6_c1, mgis_6_c2)

mgis_20_c1 <- read_csv("data/mgis/mgis_20/mgis_20_c1.csv")
mgis_20_c2 <- read_csv("data/mgis/mgis_20/mgis_20_c2.csv")
mgis_20_c1$trial_r <- "mgis_20_c1"
mgis_20_c2$trial_r <- "mgis_20_c2"
mgis_20 <- rbind(mgis_20_c1, mgis_20_c2)

mgis_6 <- correct_names(mgis_6)
unique(mgis_6$genotypes)
mgis_20 <- correct_names(mgis_20)
unique(mgis_20$genotypes)

mgis_6 <- as.data.frame(mgis_6)

mgis_20 <- as.data.frame(mgis_20)

var(mgis_6$dsi_f)
var(mgis_6$dsi_h)
hist(mgis_6$dsi_f)
hist(mgis_6$dsi_h)

#Test rankings and PL Model
mgis_6_ranks <- gosset::rank_numeric(data = mgis_6,
                             items = "genotypes",
                             input = "dsi_h",
                             id = "trial_r",
                             ascending = T)

mgis_6_ranks_mtx <- unclass(mgis_6_ranks)

mgis_6_ranks_mtx <- as.data.frame(mgis_6_ranks_mtx)

row.names(mgis_6_ranks_mtx) <- unique(mgis_6$trial_r)

mgis_6_ranks_mtx

#### MGIS_20 ####
mgis_20 <- mgis_20[!is.na(mgis_20$dsi_s), ]

var(mgis_20$dsi_s)
var(mgis_20$dsi_h)

mgis_20_ranks <- gosset::rank_numeric(data = mgis_20,
                             items = "genotypes",
                             input = "dsi_h",
                             id = "trial_r",
                             ascending = T)

mgis_20_ranks_mtx <- as.data.frame(unclass(mgis_20_ranks))

row.names(mgis_20_ranks_mtx) <- unique(mgis_20$trial_r)

mgis_20_ranks_mtx

mgis_ranks_mtx <- bind_rows(mgis_6_ranks_mtx, mgis_20_ranks_mtx)

mgis_ranks_mtx$id <- gsub(pattern = "_c1|_c2",
                          replacement = "",
                          x = rownames(mgis_ranks_mtx))



mgis_locations <- read_csv("data/mgis/mgis_locations.csv")

mgis_ranks_mtx <- merge(x = mgis_ranks_mtx,
                        y = mgis_locations,
                        by.x = "id",
                        by.y = "trial_id")

#trials change the order in rows after merge

row.names(mgis_ranks_mtx) <- c(unique(mgis_20$trial_r), 
                               unique(mgis_6$trial_r))
                               

mgis_ranks_mtx <- mgis_ranks_mtx[, -1]

mgis_ranks_mtx$id <- row.names(mgis_ranks_mtx)

colnames(mgis_ranks_mtx)

#mgis_ranks_mtx <- mgis_ranks_mtx[c(1:32, 35:37)]

mgis_ranks_mtx <- mgis_ranks_mtx[!colnames(mgis_ranks_mtx) %in% c("location_name", "country", "hdate")]

mgis_ranks_mtx$pdate <- as.Date(mgis_ranks_mtx$pdate, format = "%m/%d/%Y")



mgis_time <- c(mean(mgis_20[mgis_20$cycle == "mother", "planting_harvest"]),
               mean(mgis_20[mgis_20$cycle == "ratoon", "planting_harvest"]),
               mean(mgis_6[mgis_6$cycle == "mother", "planting_harvest"]),
               mean(mgis_6[mgis_6$cycle == "ratoon", "planting_harvest"])
               )


mgis_ranks_mtx$end_date <- mgis_ranks_mtx$pdate + mgis_time

mgis_ranks_mtx

#Correct start dates for second cycles
mgis_ranks_mtx[mgis_ranks_mtx$id == "mgis_20_c2", ]$pdate <- mgis_ranks_mtx[mgis_ranks_mtx$id == "mgis_20_c1", "end_date"]

mgis_ranks_mtx[mgis_ranks_mtx$id == "mgis_6_c2", ]$pdate <- mgis_ranks_mtx[mgis_ranks_mtx$id == "mgis_6_c1", "end_date"]

mgis_ranks_mtx <- mgis_ranks_mtx[!colnames(mgis_ranks_mtx) %in% "id"]

mgis_ranks_mtx[c("pdate","end_date")]

mgis_ranks_mtx
