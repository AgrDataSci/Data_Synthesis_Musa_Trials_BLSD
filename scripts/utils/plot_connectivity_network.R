library(gosset)
library(PlackettLuce)
library(network)
library(GGally)
library(ggnet2)
library(sna) #for function  %v% 

#First load data  and preparation lines in pltree_model


musa_ranks <- as.rankings(musa_trials_data)

musa_trials_adj <- adjacency(musa_ranks)

pl_conn <- PlackettLuce::connectivity(musa_trials_adj)

pl_conn

net <- network::network(musa_trials_adj)

types <- read.csv("data/processed/genotypes_info.csv")

types <- janitor::clean_names(types)

wild <- types[types$status  == "Wild", ]$i_name

landrace <- types[types$status  == "Landrace", ]$i_name

#improved <- c(types[types$status  == "Hybrid", ]$i_name)

improved <- c(types[types$status  == "Hybrid", ]$i_name,
              types[types$status  == "Somaclonal variant", ]$i_name)

#somaclone <- types[types$status  == "Somaclonal variant", ]$i_name

genotypes <- network::network.vertex.names(net)

genotypes

# net %v% "Type" = ifelse(genotypes %in% somaclone, "Somaclone",
#                         ifelse(genotypes %in% landrace, "Landrace", "Hybrid"))

net %v% "Status" = ifelse(genotypes %in% improved, "Improved",
                        ifelse(genotypes %in% landrace, "Landrace", "Wild"))

# type_palette_1 <- c("Landrace" = "#9acd32",
#                     "Hybrid" = "#99CC33",
#                     "Somaclone" = "#4575b4")

#                    "Released Variety" = "#99CC33")


type_palette_1 <- c("Wild" = "#87b32c",
                    "Landrace" = "orangered",
                    "Improved" = "deepskyblue4")

# type_palette_2 <- c(#"Landrace" = "#f1c40f",
#   "Experimental line" = "#e74c3c",
#   "Released Variety" = "#52be80")

GGally::ggnet2(net,
               label = TRUE,
               arrow.size = 8,
               edge.color = "gray40",
               mode = "kamadakawai",
               arrow.gap = 0.03,
               color = "Status",
               label.color = "gray5",
               palette = type_palette_1,
               legend.size = 12,
               fontface = "bold") +
  theme(legend.text = element_text(color = "gray20",face = "bold"),
        legend.title = element_text(color = "gray20", face = "bold"))


GGally::ggnet2(net,
               label = TRUE,
               arrow.size = 8,
               edge.color = "gray20",
               mode = "kamadakawai",
               arrow.gap = 0.03,
               color = "Status",
               label.color = "white",
               palette = type_palette_1,
               legend.size = 12,
               fontface = "bold") +
  theme(panel.background = element_rect(fill = "gray50"),
        plot.background = element_rect(fill = "gray50"),
        legend.background = element_rect(fill = "gray50"),
        legend.text = element_text(color = "white", face = "bold"),
        legend.title = element_text(color = "White", face = "bold"))

ggsave("output/figures/musa_trials_network.png", 
       dpi = 600,
       width = 14, 
       height = 9,
       units = "in")



