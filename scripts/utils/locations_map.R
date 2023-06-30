library(ggplot2)
library(tmap)
library(terra)
library(sf)

data("World", package = "tmap")

musa_trials_loc <- read.csv("data/processed/trial_info_ext.csv", 
                                 check.names = FALSE)

musa_trials_loc_sf <- sf::st_as_sf(musa_trials_loc,
                                   coords = c("lon", "lat"),
                                   crs = 4326)

musa_trials_loc_sf$trial <- row.names(musa_trials_loc_sf)

unq_locs_sf <- musa_trials_loc_sf[!duplicated(musa_trials_loc$lon), ]

unq_locs_sf <- unq_locs_sf[!duplicated(unq_locs_sf$location_name), ]

nrow(unq_locs_sf)

unq_locs_sf$x <- st_coordinates(unq_locs_sf)[,1]

unq_locs_sf$y <- st_coordinates(unq_locs_sf)[,2]

unq_locs_sf[unq_locs_sf$location_name == "Dole", ]$location_name <- "Kidapawan"

unq_locs_sf$id_2 <- seq(1:nrow(unq_locs_sf))

trials_p <- ggplot() + 
  geom_sf(data = World, fill = "grey25") + 
  geom_sf(data = unq_locs_sf,
          aes(color = unq_locs_sf$location_name),
          size = 2) +
  geom_sf_text(data = unq_locs_sf,
               label = unq_locs_sf$location_name,
               fontface = "bold",
               size = 3.5,
               color = "White",
               nudge_x = c(-15, 15.5, 13, 12, 11, -10, -11.5, 11.5, 10, -11.5, -10,
                           -17, 8.5, 19, -16, -12, 12.5, 15, -13.5, 13, -10, 11),
               nudge_y = c(0, 1.5, 1, 0, 2.5, -2, 2, 0, 0, 1.5, -1.5,
                           0, 4.5, 0, 3, 0, 0, 2, 0.5, 0, -4, 0)) +
  coord_sf(xlim = c(-179, 179), ylim = c(-59.5, 88), expand = F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "grey45"), 
        panel.grid.major = element_line(color = NA)) +
  scale_color_manual(values = rep("#ff4500", 22))

trials_p

ggsave("output/figures/musa_trial_locations.png", dpi = 600, width = 10, height = 5)


