"
FoT Liverpool Challenge

> name: 02_data_viz.R

> author: Adam Sonty (@sonty_1)

> functionality:
  > for each play in Last Row Liverpool 2019 Tracking Data:
      - standardize attack direction (to the right)
      
      - identify frames in which players make contact with the ball
      - label event frames
      
      - specify player roles (keeper, passer(s), shooter, off-ball)
    
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
src_data_dir <- file.path(livc_dir, "src", "data")
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
library(ggplot2)
library(gganimate)
library(ggforce)
library(ggsoccer)
library(transformr)
# files.sources = list.files(src_data_dir)
# sapply(file.path(src_data_dir, files.sources), source)

# data import =================================================================
play_files <- list.files(data_clean_dir)

play_data <- readr::read_tsv(file.path(data_clean_dir, play_files[1]))

pitch_international <- list(
  length = 100,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

play_frames <- ggplot() +
  theme_minimal() +
  annotate_pitch(colour = "white", fill = "#8abf84") +
  geom_point(
    data = play_data %>% filter(player != 0),
    aes(x = x, y = y, colour = player_role, fill = team, shape = team),
    size = 4,
    alpha = 0.9
  ) +
  geom_segment(
    data = play_data %>% filter(player != 0),
    aes(x = x, y = y, 
        xend = x + dx, yend = y + dy,
        colour = player_role, 
        ),
    arrow = arrow(length = unit(0.3,"cm"))
  ) +
  scale_shape_manual(values = c(21, 21)) +
  scale_fill_manual(values = c("#e63946", "#1d3557")) +
  scale_colour_manual(values = c("orange", "grey", "green", "yellow")) +
  geom_point(
    data = play_data %>% filter(player == 0),
    aes(x = x, y = y),
    size = 2,
    shape = 21,
    fill = "white",
    colour = "black"
  ) +
  geom_polygon(
    data = play_data %>% 
      select(frame, x, y, player_role) %>% 
      filter(player_role == "shooter") %>%
      group_by(frame) %>% 
      do(
        rbind(
          ., 
          data.frame(
            frame = c(NA, NA),
            x = c(100, 100),
            y = c(50 - 11.6/2, 50 + 11.6/2),
            player_role = rep("shooter", 2)
          )
        )
      ) %>% 
      ungroup() %>% 
      data.frame() %>% 
      tidyr::fill(frame),
    aes(x = x, y = y, group = player_role),
    fill = "white",
    alpha = 0.3
  ) +
  transition_time(frame) +
  ease_aes("linear") +
  NULL

play_len <- length(unique(play_data$frame))
play_anim <- animate(
  play_frames,
  fps = 20, 
  nframe = play_len,
  width = 1600,
  height = 920
)

play_anim
anim_save("test.gif", play_anim)














