musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1)

musa_trials_time_loc$trial_id <- row.names(musa_trials_time_loc)


unique_loc <- read.csv("data/processed/unique_locations.csv", row.names = 1)


n_node_2 <- nrow(musa_trials_time_loc_sf[musa_plt_nodes == "2", ])

n_node_3 <- nrow(musa_trials_time_loc_sf[musa_plt_nodes == "3", ])

musa_trials_time_loc$node <- rep(0, nrow(musa_trials_time_loc_sf))

musa_trials_time_loc[musa_plt_nodes == "2", ]$node <- rep(2, n_node_2)

musa_trials_time_loc[musa_plt_nodes == "3", ]$node <- rep(3, n_node_3)

musa_trials_time_loc$node <- as.factor(musa_trials_time_loc$node)

trials_node <- merge(musa_trials_time_loc, unique_loc, by.x = "trial_id", by.y = "id", order = T)

trials_node_sf <- sf::st_as_sf(trials_node,
                               coords = c("lon", "lat"),
                               crs = 4326)

trials_node_sf <- dplyr::arrange(trials_node_sf, trial)


ggplot() + 
  geom_sf(data = World, fill = "grey25") + 
  geom_sf(data = trials_node_sf, aes(color = node),
          size = 2) +
  geom_sf_text(data = trials_node_sf,
               label = trials_node_sf$location_name,
               fontface = "bold",
               size = 3.5,
               color = "White",
               # vjust = .25,
               # hjust = -.25,
               nudge_x = c(-15, 15.5, 13, 12, 11, -10, -11.5, 11.5, 10, -11.5, -10,
                           -17, 8.5, 19, -16, -12, 12.5, 15, -13.5, 13, -10, 11),
               nudge_y = c(0, 1.5, 1, 0, 2.5, -2, 2, 0, 0, 1.5, -1.5,
                           0, 4.5, 0, 3, 0, 0, 2, 0.5, 0, -4, 0)) +
          coord_sf(xlim = c(-179, 179), ylim = c(-59.5, 88), expand = F) +
          #labs(tag = "A") +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "bottom",
                legend.direction = "horizontal",
                #legend.title = element_blank(),
                #plot.tag = element_text(size = 18),
                panel.background = element_rect(fill = "grey45")) +
          scale_color_manual(values = c("blue", "orange"), 
                             labels = c("2 (Moist)", "3 (Dry)")) +
  labs(color = "Node")


ggsave("output/figures/trial_x_node.png", dpi = 600, width = 10, height = 5)



          