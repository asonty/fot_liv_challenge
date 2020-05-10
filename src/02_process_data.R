"
FoT Liverpool Challenge

> name: 02_process_data.R

> author: Adam Sonty (@sonty_1)

> functionality:
  > for a play in Last Row Liverpool 2019 Tracking Data:
      - compute shooter's metrics:
        - angle
        - distance from center of goal
        - effective goal width
        
      - compute defender metrics:
        - compute distance between defenders and ball
        - compute defenders' interception radii
        - compute defenders' interception loci
      
      - find players in shooter's goal-view
      - compute defenders' obstruction projection
      
      - get shooter's view in plotable df
      
      - get obstructors' views in plotable df
      
      - compute attack's goal-control at each frame

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
src_data_processing_dir <- file.path(livc_dir, "src", "data_processing")
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
# library(ggforce)
library(ggsoccer)
library(transformr)
# library(sp)

files.sources = list.files(src_data_processing_dir)
sapply(file.path(src_data_processing_dir, files.sources), source)

# variables ===================================================================
pitch_dims <- list(
  length = 106,
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

# data import =================================================================
play_files <- list.files(data_clean_dir)

all_plays <- tibble(
  play_file = file.path(data_clean_dir, play_files),
  def_color = c("#ebebeb", "#568bd7", "#ebebeb", "#2176ff", "#2176ff",
                "#2176ff", "#f9c80e", "#f9c80e", "#2176ff", "#a0cbff",
                "#f9c80e", "#f9c80e", "#2176ff", "#ebebeb", "#568bd7",
                "#f9c80e", "#f9c80e", "#f9c80e", "#e6f14a"
  )
)

for (i in 1:nrow(all_plays)) {
  row <- all_plays[i, ]
  play_data <- readr::read_tsv(row$play_file)
  def_color <- row$def_color
  
  shooter_metrics <- compute_shooter_metrics(play_data)
  def_metrics <- compute_defender_metrics(play_data)
  obstruct_poly <- find_obstructors(play_data, def_metrics)
  shooter_view <- get_shooter_view(play_data)
  obstruct_views <- get_obstructor_views(obstruct_poly)
  shooter_gc <- compute_goal_control(play_data, obstruct_poly)
  
  #  find frame of shot
  shot_frame <- play_data %>% filter(player_action == "shot") %>% .$frame
  score_frame <- play_data %>% filter(x > pitch_dims$length & player == 0) %>% .$frame %>% first()
  #  reduce play dataset to 3 seconds before shot
  min_play_data <- play_data %>% 
    filter(frame >= shot_frame - 60 & frame <= score_frame + 5)
  
  this_frame <- 136#98
  
  # #----
  # ggplot() +
  #   theme_dark() +
  #   annotate_pitch(colour = "#403d39", fill = "#242424", dimensions = pitch_dims) +
  #   labs(
  #     title = play_data$play[1]
  #   ) +
  #   # shooter view
  #   geom_polygon(
  #     data = shooter_view %>% filter(frame == this_frame),
  #     aes(
  #       x = x, 
  #       y = -(y - pitch_dims$width)
  #     ),
  #     fill = "#f25c54",
  #     alpha = 0.5
  #   ) +
  #   # obstructors views
  #   geom_polygon(
  #     data = obstruct_views %>% filter(frame == this_frame),
  #     aes(
  #       x = x, 
  #       y = -(y - pitch_dims$width), 
  #       group = player
  #     ),
  #     fill = "#242424",
  #     alpha = 0.7
  #   ) +
  #   # attackers velocities
  #   geom_segment(
  #     data = play_data %>% filter(team == "attack" & frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y- pitch_dims$width),
  #       xend = x + vx,
  #       yend = -(y + vy - pitch_dims$width),
  #     ),
  #     colour = "#e01e37",
  #     arrow = arrow(length = unit(0.015,"npc"))
  #   ) +
  #   # defenders velocities
  #   geom_segment(
  #     data = play_data %>% filter(team == "defense" & frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #       xend = x + vx,
  #       yend = -(y + vy - pitch_dims$width),
  #     ),
  #     colour = "#f9c80e",
  #     arrow = arrow(length = unit(0.015,"npc"))
  #   ) +
  #   # defenders "side wings"
  #   geom_segment(
  #     data = def_metrics %>% filter(frame == this_frame),
  #     aes(
  #       x = lw_x,
  #       y = -(lw_y - pitch_dims$width),
  #       xend = rw_x,
  #       yend = -(rw_y - pitch_dims$width),
  #     ),
  #     colour = "grey",
  #   ) +
  #   # defenders "forward wings"
  #   geom_segment(
  #     data = def_metrics %>% filter(frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #       xend = fwd_x,
  #       yend = -(fwd_y - pitch_dims$width),
  #     ),
  #     colour = "grey",
  #   ) +
  #   # attackers
  #   geom_point(
  #     data = play_data %>% filter(team == "attack" & frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #     ),
  #     # shape = 21,
  #     size = 5,
  #     colour = "#e01e37"
  #   ) +
  #   # defenders
  #   geom_point(
  #     data = play_data %>% filter(team == "defense" & frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #     ),
  #     # shape = 21,
  #     size = 5,
  #     colour = "#f9c80e"
  #   ) +
  #   # attacker numbers
  #   geom_text(
  #     data = play_data %>% filter(frame == this_frame) %>% tidyr::drop_na(player_num),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #       label = player_num
  #     ),
  #     size = 3.5,
  #     colour = "white",
  #     family = "Lekton"
  #   ) +
  #   # ball
  #   geom_point(
  #     data = play_data %>% filter(player == 0 & frame == this_frame),
  #     aes(
  #       x = x,
  #       y = -(y - pitch_dims$width),
  #     ),
  #     shape = 21,
  #     size = 2,
  #     colour = "white",
  #     fill = "black"
  #   ) +
  #   # effective goal width
  #   geom_text(
  #     data = shooter_metrics %>% filter(frame == this_frame),
  #     aes(
  #       x = pitch_dims$length/2 + 3,
  #       y = pitch_dims$width - 1,
  #       label = paste("eff. goal width:", formatC(eff_goal_width, digits=2, width=4, format = "f"))
  #     ), 
  #     hjust = 1,
  #     vjust = 0,
  #     size = 5,
  #     color = "#5aa9e6",
  #     family = "Lekton"
  #   ) +
  #   # goal control per frame
  #   geom_text(
  #     data = shooter_gc %>% filter(frame == this_frame),
  #     aes(
  #       x = pitch_dims$length/2 + 2,
  #       y = pitch_dims$width - 1,
  #       label = paste("goal control:", formatC(attack_gc, digits=2, width=4, format = "f"))
  #     ), 
  #     hjust = 1,
  #     vjust = 1,
  #     size = 5,
  #     color = "#06d6a0",
  #     family = "Lekton"
  #   ) +
  #   coord_flip(
  #     xlim = c(pitch_dims$length/2, pitch_dims$length),
  #     ylim = c(0, pitch_dims$width)
  #   ) +
  #   NULL
  
  # ----
  play_frames <- ggplot() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#242424") ,
      plot.title = element_text(family = 'Roboto Light', size = 25, color = '#fafafa')
    ) +
    labs(
      title = play_data$play[1]
    ) +
    annotate_pitch(colour = "#5e5e5e", fill = "#242424", dimensions = pitch_dims) +
    # shooter view
    geom_polygon(
      data = shooter_view,
      aes(
        x = x, 
        y = -(y - pitch_dims$width)
      ),
      fill = "#f25c54",
      alpha = 0.5
    ) +
    # obstructors views
    geom_polygon(
      data = obstruct_views,
      aes(
        x = x, 
        y = -(y - pitch_dims$width), 
        group = player
      ),
      fill = def_color,
      alpha = 0.7
    ) +
    # attackers velocities
    geom_segment(
      data = play_data %>% filter(team == "attack"),
      aes(
        x = x,
        y = -(y- pitch_dims$width),
        xend = x + vx,
        yend = -(y + vy - pitch_dims$width),
      ),
      colour = "#e01e37",
      arrow = arrow(length = unit(0.015,"npc"))
    ) +
    # defenders velocities
    geom_segment(
      data = play_data %>% filter(team == "defense"),
      aes(
        x = x,
        y = -(y - pitch_dims$width),
        xend = x + vx,
        yend = -(y + vy - pitch_dims$width),
      ),
      colour = def_color,
      arrow = arrow(length = unit(0.015,"npc"))
    ) +
    # defenders "side wings"
    geom_segment(
      data = def_metrics,
      aes(
        x = lw_x,
        y = -(lw_y - pitch_dims$width),
        xend = rw_x,
        yend = -(rw_y - pitch_dims$width),
      ),
      colour = "grey",
    ) +
    # defenders "forward wings"
    geom_segment(
      data = def_metrics,
      aes(
        x = x,
        y = -(y - pitch_dims$width),
        xend = fwd_x,
        yend = -(fwd_y - pitch_dims$width),
      ),
      colour = "grey",
    ) +
    # attackers
    geom_point(
      data = play_data %>% filter(team == "attack"),
      aes(
        x = x,
        y = -(y - pitch_dims$width),
      ),
      # shape = 21,
      size = 5,
      colour = "#e01e37"
    ) +
    # defenders
    geom_point(
      data = play_data %>% filter(team == "defense"),
      aes(
        x = x,
        y = -(y - pitch_dims$width),
      ),
      # shape = 21,
      size = 5,
      colour = def_color
    ) +
    # attacker numbers
    geom_text(
      data = play_data %>% tidyr::drop_na(player_num),
      aes(
        x = x,
        y = -(y - pitch_dims$width),
        label = player_num
      ),
      size = 3.5,
      colour = "white",
      family = "Lekton"
    ) +
    # ball
    geom_point(
      data = play_data %>% filter(player == 0),
      aes(
        x = x,
        y = -(y - pitch_dims$width),
      ),
      shape = 21,
      size = 3,
      colour = "white",
      fill = "#61f2c2"
    ) +
    # shooter angle
    geom_text(
      data = shooter_metrics,
      aes(
        x = pitch_dims$length/2 + 5,
        y = pitch_dims$width - 1,
        label = paste("shooter angle:", formatC(goal_theta_deg, digits=1, width=3, format = "f"))
      ), 
      hjust = 1,
      vjust = 0,
      size = 7,
      color = "#ffffff",
      family = "Lekton"
    ) +
    # effective goal width
    geom_text(
      data = shooter_metrics,
      aes(
        x = pitch_dims$length/2 + 3,
        y = pitch_dims$width - 1,
        label = paste("eff. goal width:", formatC(eff_goal_width, digits=2, width=4, format = "f"))
      ), 
      hjust = 1,
      vjust = 0,
      size = 7,
      color = "#ff8a5b",
      family = "Lekton"
    ) +
    # goal control per frame
    geom_text(
      data = shooter_gc,
      aes(
        x = pitch_dims$length/2 + 2,
        y = pitch_dims$width - 1,
        label = paste("goal control:", formatC(attack_gc, digits=2, width=4, format = "f"))
      ), 
      hjust = 1,
      vjust = 1,
      size = 7,
      color = "#80ed99",
      family = "Lekton"
    ) +
    coord_flip(
      xlim = c(pitch_dims$length/2, pitch_dims$length),
      ylim = c(0, pitch_dims$width)
    ) +
    transition_manual(frame) +
    ease_aes("linear") +
    NULL
  
  play_len <- length(unique(play_data$frame))
  play_anim <- animate(
    play_frames,
    fps = 20, 
    nframe = play_len,
    width = 1000,
    height = 700,
    renderer = ffmpeg_renderer()
  )
  
  
  # play_anim
  anim_save(paste(janitor::make_clean_names(play_data$play[1]), ".mp4", sep=""), play_anim)
  
  
  
  
}
