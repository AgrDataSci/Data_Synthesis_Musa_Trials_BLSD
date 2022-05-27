library(gosset)
library(PlackettLuce)
library(ggplot2)
library(ggparty)
library(patchwork)
library(caret)
library(stablelearner)
library(ggrepel)

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


plot_tree_musa(musa_plt_01, log = T , qve = T, ref = "Calcutta 4")

ggsave("output/figures/musa_trial_plt_01.png", dpi = 800, width = 14, height = 18, units = "in")

#sampling with 80%
set.seed(123)
stb_01 <- stablelearner::stabletree(musa_plt_01,
                                    sampler = stablelearner::subsampling, 
                                    savetrees = TRUE, 
                                    B = 1000, 
                                    v = .8)

#save(stb_01, file = "output/models/stb_01.rda")

#load(file = "output/models/stb_01.rda")

summary(stb_01)

png(filename = "output/figures/stb_01_vsf.png", width = 16, height = 10, res = 600,units = "in")
barplot(stb_01)
dev.off()

png("output/figures/stb_01_vs.png",  width = 16, height = 10, res = 600,units = "in")
image(stb_01)
dev.off()

#plot(stb_01, select = c("MLDS", "SDII", "rhum_09", "CSDI", "DTR"))

png("output/figures/stb_01_cop.png",  width = 16, height = 10, res = 600,units = "in")
plot(stb_01, select = c("MLDS"))
dev.off()


rel_freqs <- colSums(stb_01$vs) / 1000 * 100

rel_freqs_df <- data.frame("variable" = names(rel_freqs), "rel_freq" = rel_freqs)

rel_freqs_df$initial <- ifelse(rel_freqs_df$variable == "MLDS", "yes", "no") 

rel_freqs_df <- rel_freqs_df[rel_freqs_df$rel_freq > 0, ]

#

ggplot() + 
  geom_col(data = rel_freqs_df, aes(x = reorder(variable, -rel_freq),
                                    y = rel_freq,
                                    fill = initial)) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1)) + 
  labs(x = "Variables",
       y = "Frequency (%)") + 
  scale_fill_manual(values = c("yes" = "#b10026", "no" = "gray35"))


ggsave("output/figures/rel_freq_stb_01.png", dpi = 300, width = 8, height = 4)


#####################################################
#Figure 4
genotypes_info <- read.csv("data/processed/genotypes_info.csv")

genotypes_info <- janitor::clean_names(genotypes_info)

qv_plt_01 <- qvcalc2(musa_plt_01, log = TRUE, ref = "Calcutta 4")

qv_plt_01
nrow(qv_plt_01[[1]])
nrow(qv_plt_01[[2]])

qv_plt_01[[1]] <- reliability2(qv_plt_01[[1]])



node_2_summ <- dplyr::arrange(qv_plt_01[[1]], desc(estimate))[1:10, ]
node_2_summ

write.table(node_2_summ,
            "output/node_2_summ.csv",
            sep = ",",
            row.names = FALSE)

qv_plt_01[[2]] <- reliability2(qv_plt_01[[2]])

node_3_summ <- dplyr::arrange(qv_plt_01[[2]], desc(estimate))[1:10, ]
node_3_summ

write.table(node_3_summ,
            "output/node_3_summ.csv",
            sep = ",",
            row.names = FALSE)


plt_coefs <-  data.frame("genotype" = names(itempar(musa_plt_01, vcov = F, log = T, ref = 1)[1, ]),
                         "node_2" = itempar(musa_plt_01, vcov = F, log = T, ref = 1)[1, ], 
                         "node_3" = itempar(musa_plt_01, vcov = F, log = T, ref = 1)[2, ])

plt_coefs <- merge(plt_coefs, genotypes_info, by.x = "genotype", by.y = "i_name")

gen_pal_txt <- ifelse(plt_coefs$status == "Wild",
                      "navy",
                      ifelse(plt_coefs$status == "Landrace",
                             "#b10026",
                             "gray15"))


plt_coefs$status

ggplot() + 
  geom_point(data = plt_coefs, 
             aes(x = node_3,
                 y = node_2)) +
  geom_text_repel(data = plt_coefs, 
                  aes(x = node_3, 
                      y = node_2, 
                      label = genotype,
                      color = status),
                  size = 5,
                  max.overlaps = 20) +
  geom_hline(yintercept = 0,
             linetype = 'dashed',
             color = "gray40") +
  geom_vline(xintercept = 0,
             linetype = 'dashed',
             color = "gray40") + 
  #geom_abline() +
  labs(x = "Node 3 (Dry)",
       y = "Node 2 (Moist)") +
  scale_color_manual(values = c("gray25", "#b10026","#b10026","navy")) +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") 
  

ggsave(filename = "output/figures/worth_nodes.png",
       dpi = 600, width = 14, height = 12)

################################



