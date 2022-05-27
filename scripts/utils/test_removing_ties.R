#Test if the effect of removing the tied genotypes

library(gosset)
library(PlackettLuce)
library(ggplot2)
library(ggparty)
library(patchwork)
library(caret)
library(stablelearner)
library(ggrepel)

pltrees <- vector(mode = "list", length = 1000)
set.seed(12345)
for(j in seq_along(pltrees)){

source(file = "scripts/utils/plot_tree_musa_trials.R")
source(file = "scripts/utils/node_reliability.R")
source(file = "scripts/utils/qvcalc2.R")

#load trial and climate data
source("scripts/utils/load_data.R")

genotypes_info <- read.csv("data/processed/genotypes_info.csv")

genotypes_info <- janitor::clean_names(genotypes_info)

head(musa_trials_data)

head(musa_trials_climate)

total_genotypes <- ncol(musa_trials_data)

total_genotypes

sort(colnames(musa_trials_data))

musa_trials_climate <- musa_trials_climate[, -1]

#remove variables with near zero variability

near_zero_var <- caret::nearZeroVar(musa_trials_climate, names = TRUE)

near_zero_var

musa_trials_climate <- musa_trials_climate[!colnames(musa_trials_climate) %in% near_zero_var]


#using default cutoff = .9
correlated <- caret::findCorrelation(cor(musa_trials_climate),
                                     names = TRUE,
                                     cutoff = .9)

correlated

#musa_trials_climate <- musa_trials_climate[!colnames(musa_trials_climate) %in% correlated]

sort(colnames(musa_trials_climate))

# png(filename = "output/figures/covariates_correlation.png",
#     width = 10, height = 5, res = 300, units = "in")
# 
# pheatmap::pheatmap(cor(musa_trials_climate, method = "pearson"),
#                    display_numbers = TRUE,
#                    number_color = "gray8",
#                    angle_col = 45,
#                    cluster_rows = FALSE,
#                    cluster_cols = FALSE,
#                    fontsize_number = 8,
#                    fontsize = 8
#                    )
#  
# dev.off()

#Check network connectivity

musa_ranks <- as.rankings(musa_trials_data)

musa_trials_adj <- adjacency(musa_ranks)

pl_conn <- PlackettLuce::connectivity(musa_trials_adj)

pl_conn

unconnected <- which(pl_conn$membership != 1)

unconnected

#Remove unconnecte genotypes
musa_trials_data <- musa_trials_data[,!colnames(musa_trials_data) %in% names(unconnected)]
# nrow(musa_trials_data)
# ncol(musa_trials_data)

#musa_trials_data <- musa_trials_data[,!colnames(musa_trials_data) %in% genotypes_remove]

#### Check and remove ties ####
best_genotype <- vector(mode = "list", length = nrow(musa_trials_data)) 
worst_genotype <- vector(mode = "list", length = nrow(musa_trials_data))

tied_best <- vector(mode = "numeric", length = nrow(musa_trials_data))

tied_worst <- vector(mode = "numeric", length = nrow(musa_trials_data))

for(i in 1:nrow(musa_trials_data)){
  
  best_genotype[[i]] <-  which(musa_trials_data[i, ] == 1)
  tied_best[[i]] <- ifelse(length(best_genotype[[i]]) > 1, i, NA) 
  worst_genotype[[i]] <-  which(musa_trials_data[i,] == max(musa_trials_data[i, ]))
  tied_worst[[i]] <- ifelse(length(worst_genotype[[i]]) > 1, i, NA) 
  
}

best_genotype
worst_genotype

tied_best <- which(!is.na(tied_best))
tied_worst <- which(!is.na(tied_worst))

tied_best

tied_worst

set.seed(123)
#Remove genotypes tied in last position
for(i in seq_len(nrow(musa_trials_data))){
  
  print(i)
  
  dup_i <- musa_trials_data[i, which(duplicated(musa_trials_data[i, ]) &
                                       musa_trials_data[i, ] > 0)]
  
  
  #unique(dup_i)
  tied_i_groups <- lapply(X = unique(dup_i),
                          FUN = function(X){
                            names(musa_trials_data[i,musa_trials_data[i,] == X])})
  
  
  tied_i_remove <- unlist(lapply(X = tied_i_groups,
                                 FUN = function(X){
                                   sample(X, length(X)-1)}))
  
  
  
  musa_trials_data[i, tied_i_remove] <- 0
  
}

# ncol(musa_trials_data)
# nrow(musa_trials_data)
# length(musa_trials_data[i,])

#### Check and remove underrepresented genotypes

sort(colSums(musa_trials_data == 0))

sort(round(colSums(musa_trials_data == 0)/nrow(musa_trials_data) * 100), decreasing = T)

absence_perc <- round(colSums(musa_trials_data == 0)/nrow(musa_trials_data) * 100)

sort(absence_perc)

#Keep only genotypes present in at least 95% of the rankings
to_keep <- names(which(absence_perc <= 95))

musa_trials_data <- musa_trials_data[, colnames(musa_trials_data) %in% to_keep]
musa_trials_data

#remove_empty <- which(rowSums(musa_trials_data) == 0)

which(rowSums(musa_trials_data) == 0)

#### Fit Plackett-Luce tree ####

sort(colnames(musa_trials_data))
ncol(musa_trials_data)
nrow(musa_trials_data)

musa_ranks <- as.rankings(musa_trials_data)

musa_ranks <- group(musa_ranks, seq_along(musa_ranks))

musa_data <- cbind(musa_ranks, musa_trials_climate)

#musa_data <- musa_data[-remove_empty, ]

musa_plt_01 <- pltree(musa_ranks ~ .,
                      data = musa_data,
                      npseudo = 2,
                      alpha = 0.05,
                      bonferroni = T,
                      verbose = TRUE,
                      ncores = 7,
                      minsize = round(nrow(musa_data) * .35))

pltrees[[j]] <- pltree(musa_ranks ~ .,
                      data = musa_data,
                      npseudo = 2,
                      alpha = 0.05,
                      bonferroni = T,
                      verbose = TRUE,
                      ncores = 7,
                      minsize = round(nrow(musa_data) * .35))

}

split_vars <- unlist(lapply(pltrees, gosset::node_labels))

table(split_vars)
# 
# save(pltrees, file = "output/random_tied_genotypes_sim.rda")