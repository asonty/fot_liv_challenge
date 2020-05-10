"
FoT Liverpool Challenge

> name: 01_clean_data.R

> author: Adam Sonty (@sonty_1)

> functionality:
  > for each play in Last Row Liverpool 2019 Tracking Data:
      - standardize attack direction (to the right)
      
      - identify frames in which players make contact with the ball
      - label event frames
      
      - specify player roles (keeper, passer(s), shooter, off-ball)
      
      - rescale coordinates to pitch size (106m x 68m) 
      
      - TODO compute instantaneous velocities for each frame
    
      - write cleaned play data to .tsv
      
> dependencies:
  + dplyr
  + janitor
  + readr
  + tidyr
"

# directories =================================================================
# * main dirs ----
soccer_analytics_dir <- "~/proj/soccer_analytics"
fotd_dir <- file.path(soccer_analytics_dir, "fotd")
livc_dir <- file.path(fotd_dir, "liv_challenge")
src_data_cleaning_dir <- file.path(livc_dir, "src", "data_cleaning")
data_raw_dir <- file.path(
  soccer_analytics_dir, 
  "data",
  "last_row", 
  "datasets",
  "positional_data"
)
data_clean_dir <- file.path(
  livc_dir,
  "data",
  "clean"
)

setwd(soccer_analytics_dir)

# dependencies ================================================================
print("==== loading dependencies ====")
library(dplyr)
files.sources = list.files(src_data_cleaning_dir)
sapply(file.path(src_data_cleaning_dir, files.sources), source)

# data import =================================================================
print("==== importing data ====")
data_files <- list.files(path = data_raw_dir,
                         full.names = TRUE,
                         recursive = TRUE)

#  read tracking data into df
liv_2019 <- suppressWarnings(
  readr::read_csv(
    data_files[2]
  )
)

#  get df listing all plays in tracking data
all_plays <- liv_2019 %>%
  select(play) %>% 
  distinct()

# data cleaning ===============================================================
print("==== cleaning data ====")
for (i in 1:nrow(all_plays)) {
  curr_play <- all_plays[i, ]$play
  print(curr_play)
  
  play_data <- liv_2019 %>% 
    filter(play == curr_play) %>%
    rename("key" = "X1")
  
  play_data <- standardize_play_direction(play_data)
  play_data <- generate_event_data(play_data)
  play_data <- specify_player_roles(play_data)
  play_data <- rescale_coordinates(play_data)
  play_data <- compute_velocities(play_data)
  
  processed_play_file <- paste(
    data_clean_dir,
    "/", janitor::make_clean_names(curr_play),
    ".tsv",
    sep = ""
  )
  
  readr::write_tsv(play_data, processed_play_file)

  print("")
}



# test by visualization ----------------------

# library(ggplot2)
# library(ggsoccer)
# library(ggforce)
# 
# shot_frame <- play_data[play_data$player_action == "shot", ]$frame
# this_frame <- 160
# 
# field_outline <- data.frame(x = c(0, 100, 100, 0),
#                             y = c(0, 0, 68, 68))
# 
# shooter_view <- play_data %>%
#   filter(frame == this_frame & player_role == "shooter") %>%
#   select(x, y, player_role) %>%
#   bind_rows(
#     tibble(
#       x = c(100, 100),
#       y = c(34 - 7.32/2, 34 + 7.32/2),
#       player_role = rep("shooter", 2)
#     )
#   )
# pitch_international <- list(
#   length = 100,
#   width = 68,
#   penalty_box_length = 16.5,
#   penalty_box_width = 40.32,
#   six_yard_box_length = 5.5,
#   six_yard_box_width = 18.32,
#   penalty_spot_distance = 11,
#   goal_width = 7.32,
#   origin_x = 0,
#   origin_y = 0
# )
# 
# ggplot() +
#   theme_minimal() +
#   annotate_pitch(colour = "white", fill = "#7d967a", dimensions = pitch_international) +
#   # geom_voronoi_tile(
#   #   data = play_data %>% filter(frame == this_frame & team != "ball"),
#   #   aes(x = x, y = y, group = -1L, fill = team),
#   #   bound = field_outline,
#   #   alpha = 0.4,
#   #   colour = "#dbdbdb"
#   # ) +
#   geom_point(
#     data = play_data %>% filter(frame == this_frame & team != "ball"),
#     aes(x = x, y = y, colour = player_role, fill = team, shape = team),
#     size = 2,
#     alpha = 0.9
#   ) +
#   scale_shape_manual(values = c(21, 21)) +
#   scale_fill_manual(values = c("#e63946", "#1d3557")) +
#   scale_colour_manual(values = c("orange", "grey", "green", "yellow")) +
#   # geom_text(
#   #   data = play_data %>% filter(frame == this_frame) %>% tidyr::drop_na(player_num),
#   #   aes(
#   #     x = x,
#   #     y = y,
#   #     label = player_num
#   #   ),
#   #   size = 4,
#   #   colour = "white",
#   #   family = "Lekton"
#   # ) +
#   geom_point(
#     data = play_data %>% filter(frame == this_frame & team == "ball"),
#     aes(x = x, y = y),
#     size = 1,
#     colour = "black"
#   ) +
#   geom_polygon(
#     data = shooter_view,
#     aes(x = x, y = y, group = player_role),
#     fill = "yellow",
#     alpha = 0.3
#   )

# pitch_opta$goal_width
# 
# pitch_opta$length

