library(ggplot2)
library(tmap)
library(terra)
library(sf)
library(colorspace)
library(patchwork)
library(reshape2)
library(RColorBrewer)


# RColorBrewer::brewer.pal(12, "Set3")
# display.brewer.pal(n = 12, name = "Paired")[1:]
# 
# display.brewer.pal(n = 8, name = "Set1")
# display.brewer.pal(n = 8, name = "Dark2")
# 
# RColorBrewer::brewer.pal(9, "Set1")

clim_colpal <- c(RColorBrewer::brewer.pal(5, "Set1"), "#10B6B1", "#A65628", "#0808F9",
                 RColorBrewer::brewer.pal(7, "Dark2"),"limegreen",
                 "#710175", "#105573", "#436605", "#D7430D", "navy", "orangered")


clim_colpal_clust <- c(RColorBrewer::brewer.pal(5, "Set1"),
                       "#710175", "#105573", "#436605", "#D7430D", "navy")


data("World")

musa_trials_loc <- read.csv("data/processed/trial_info_ext.csv", 
                                 check.names = FALSE)

#imtp_locs <- read.csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

musa_trials_loc_sf <- sf::st_as_sf(musa_trials_loc,
                                   coords = c("lon", "lat"),
                                   crs = 4326)

musa_trials_loc_sf$trial <- row.names(musa_trials_loc_sf)

unq_locs_sf <- musa_trials_loc_sf[!duplicated(musa_trials_loc$lon), ]

unq_locs_sf <- unq_locs_sf[!duplicated(unq_locs_sf$location_name), ]

nrow(unq_locs_sf)

unq_locs_sf$x <- st_coordinates(unq_locs_sf)[,1]

unq_locs_sf$y <- st_coordinates(unq_locs_sf)[,2]

#merge(unq_locs_sf, imtp_locs, by.x = c("x","y"), by.y = c("longitude","latitude"))

#unq_locs_sf <- unq_locs_sf[!unq_locs_sf$location_name == "Dole", ]

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
        panel.background = element_rect(fill = "grey45")) +
  scale_color_manual(values = rep("#ff4500", 22))

trials_p

ggsave("output/figures/musa_trial_locations.png", dpi = 600, width = 10, height = 5)


#Temperture
temp_files <- list.files("data/ecv_ERA5/temp/", full.names = TRUE)

temp_stack <- terra::rast(temp_files)

temp_stack <- temp_stack - 273.15

dim(temp_stack)

temp_stack <- terra::rotate(temp_stack)

temp_trials <- terra::extract(temp_stack, terra::vect(unq_locs_sf), list = F)

colnames(temp_trials)[grepl(pattern = "SFC", colnames(temp_trials))] <- month.abb#c("id", month.abb)

unq_locs_sf$temp <- rowMeans(temp_trials[-1])

 
# temp_trials <- merge(x = unq_locs_sf, y = temp_trials, by.x = "id", by.y = "ID")
# 
# temp_trials_df <- as.data.frame(temp_trials)
# 
# sel_cols <- c("id", "country", "location_name", "study", month.abb)
# 
# temp_trials_df <- temp_trials_df[sel_cols]
# 
# 
# temp_trials_df_l <- reshape2::melt(temp_trials_df, 
#                          id.vars = c("location_name"), 
#                          measure.vars = month.abb)
# temp_trials_df_l
# 
# temp_p <- ggplot() + 
#   geom_line(data = temp_trials_df_l, aes(x = variable,
#                                          y = value,
#                                          col = as.factor(location_name),
#                                          group = as.factor(location_name)
#                                          ), size = 1.35) + 
#   # geom_point(data = temp_trials_df_l, aes(x = variable,
#   #                                         y = value,
#   #                                         col = as.factor(id))) +
#   labs(x = "",
#        y = "°C",
#        title = "Average temperature 1990 - 2020",
#        color = "Location name") +
#   scale_color_manual(values = clim_colpal) + 
#   theme(#legend.position = "none",
#         panel.background = element_rect(fill = "gray60"),
#         panel.grid = element_line(color = "gray30"))
# 
# temp_p


#prec 

prec_files <- list.files("data/ecv_ERA5/prec/", full.names = TRUE)

prec_stack <- terra::rast(prec_files)

prec_stack <- prec_stack * 1000 * 30

dim(prec_stack)

prec_stack <- terra::rotate(prec_stack)

prec_trials <- terra::extract(prec_stack, terra::vect(unq_locs_sf), list = F)

colnames(prec_trials)[grepl(pattern = "SFC", colnames(prec_trials))] <- month.abb#c("id", month.abb)

unq_locs_sf$prec <- rowMeans(prec_trials[-1])

prec_trials$trial_id <- unq_locs_sf$id


prec_trials <- merge(x = unq_locs_sf, y = prec_trials, by.x = "id", by.y = "trial_id", order = F)

colnames(prec_trials)

prec_trials <- sf::st_drop_geometry(prec_trials)

prec_trials_df <- as.data.frame(prec_trials)

sel_cols <- c("id", "country", "location_name", "study", month.abb)

prec_trials_df <- prec_trials_df[sel_cols]


which(duplicated(prec_trials_df$location_name))

prec_trials_df_l <- melt(prec_trials_df,
                         id.vars = c("location_name"),
                         measure.vars = month.abb)
prec_trials_df_l

prec_p <- ggplot() +
  geom_line(data = prec_trials_df_l, aes(x = variable,
                                         y = value,
                                         col = as.factor(location_name),
                                         linetype = as.factor(location_name),
                                         group = as.factor(location_name)
  ), size = 1.35) +
  # geom_point(data = prec_trials_df_l, aes(x = variable,
  #                                         y = value,
  #                                         col = as.factor(id))) +
  labs(x = "",
       y = "mm",
       title = "Average precipitation 1990 - 2020",
       color = "Location name") +
  scale_color_manual(values = clim_colpal) +
   scale_linetype_manual(values = c(rep("dashed", 7),
                                    rep("solid", 15)))
  # theme(legend.position = "none")

prec_p

#rhum

rhum_files <- list.files("data/ecv_ERA5/rhum/", full.names = TRUE)

rhum_stack <- terra::rast(rhum_files)

rhum_stack <- rhum_stack

dim(rhum_stack)

rhum_stack <- terra::rotate(rhum_stack)

rhum_trials <- terra::extract(rhum_stack, terra::vect(unq_locs_sf), list = F)

colnames(rhum_trials)[grepl(pattern = "SFC", colnames(rhum_trials))] <- month.abb#c("id", month.abb)

unq_locs_sf$rhum <- rowMeans(rhum_trials[-1])

# rhum_trials <- merge(x = unq_locs_sf, y = rhum_trials, by.x = "id", by.y = "ID")
# colnames(rhum_trials)
# 
# 
# 
# rhum_trials_df <- as.data.frame(rhum_trials)
# 
# sel_cols <- c("id", "country", "location_name", "study", month.abb)
# 
# rhum_trials_df <- rhum_trials_df[sel_cols]
# 
# which(duplicated(rhum_trials_df$location_name))
# 
# rhum_trials_df_l <- melt(rhum_trials_df, 
#                          id.vars = c("location_name"), 
#                          measure.vars = month.abb)
# rhum_trials_df_l
# 
# rhum_p <- ggplot() + 
#   geom_line(data = rhum_trials_df_l, aes(x = variable,
#                                          y = value,
#                                          col = as.factor(location_name),
#                                          group = as.factor(location_name)
#   ), size = 1.35) + 
#   # geom_point(data = rhum_trials_df_l, aes(x = variable,
#   #                                         y = value,
#   #                                         col = as.factor(id))) +
#   labs(x = "",
#        y = "%",
#        title = "Average relative humidity 1990 - 2020",
#        color = "Location name") +
#   scale_color_manual(values = clim_colpal) 
#   
# 
# rhum_p
# 
# 
# (temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect') + 
#   theme(legend.key.size = unit(1.25, 'cm'),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 14)) +
#   plot_annotation(tag_levels = list(c("B", "C", "D" )))
  
# +
#   theme(legend.position = "bottom",
#         legend.direction = "horizontal")

# ggsave("output/figures/trials_clim.png", dpi = 600, width = 10, height = 10)
# 
# trials_p |  (temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect',
#                                                      ncol = 2,
#                                                      #heights = c(2, 1, 1, 1),
#                                                      widths = c(1,2,2,2))
# 
# 
# trials_p #/  (temp_p / prec_p / rhum_p) + plot_layout(guides = 'collect')
# 
# ggsave("output/figures/trials_locations.png", dpi = 600, width = 16, height = 12)



write.csv(st_drop_geometry(unq_locs_sf), file = "data/processed/unique_locations.csv")

#############################################################



