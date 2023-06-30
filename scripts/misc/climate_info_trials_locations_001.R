
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



