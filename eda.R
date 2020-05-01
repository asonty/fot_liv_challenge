"
FoT Liverpool Challenge EDA
"

# =============================================================================
# directories
# =============================================================================
# * main dirs ----
soccer_analytics_dir <- '~/proj/soccer_analytics'
livc_dir <- file.path(soccer_analytics_dir, 'liv_challenge')
data_dir <- file.path(soccer_analytics_dir, 'Last-Row', 'datasets', 'positional_data')

# * paths to SB open data files ----

setwd(soccer_analytics_dir)

# dependencies ================================================================
library(tidyverse)
library(ggsoccer)
library(gganimate)
# library(ggvoronoi)
library(ggforce)
library(glue)
library(janitor)

# library(StatsBombR)
# library(jsonlite)
# library(SBpitch)

# data loading ================================================================

data_files <- list.files(path = data_dir,
                         full.names = TRUE,
                         recursive = TRUE)

liv_che <- readr::read_csv(data_files[1])

liv_2019 <- readr::read_csv(
  data_files[2],
  col_types = cols(X1 = col_skip())
)

all_plays <- liv_2019 %>% select(play) %>% distinct()

# helper functions ============================================================

# * get play data

# * get players who touched the ball

# * get goal scorer

# * plot frame

# * create gif of play


for (i in 1:nrow(all_plays)) {
  play_ <- all_plays[i, ]$play
  print(play_)
  play_file <- glue("{make_clean_names(play_)}.gif")
  
  play_data <- liv_2019 %>% filter(play == play_)
  
  play_frames <- ggplot() +
    theme_minimal() +
    annotate_pitch() +
    geom_point(
      data = play_data,
      aes(
        x = x,
        y = y,
        color = team
      )
    ) +
    transition_time(frame) +
    ease_aes("linear") +
    NULL
  
  play_len <- length(unique(play_data$frame))
  play_anim <- animate(
    play_frames,
    fps = 20, 
    nframe = play_len,
    width = 850,
    height = 490
  )
  
  anim_save(play_file, play_anim)
}

play <- liv_2019 %>% filter(play == "Liverpool [4] - 0 Barcelona")

field_outline <- data.frame(x = c(0, 100, 100, 0),
                            y = c(0, 0, 100, 100))

ggplot(data = play %>% filter(frame == 4*20)) +
  annotate_pitch(colour = 'white', fill = '#7d967a') +
  geom_point(
    aes(x = x, y = y, color = team)
  ) +
  geom_voronoi(
    aes(x = x, y = y, fill = team),
    alpha = 0.2,
    color = "white",
    outline = field_outline
  )

dupes <- play %>%
  get_dupes(x, y, frame)

shot_fr <- 122

# create frame plot
ggplot() +
  annotate_pitch(colour = 'white', fill = 'gray') +
  geom_voronoi_tile(
    data = play %>% filter(frame == shot_fr) %>% drop_na("team"),
    aes(x = x, y = y, group = -1L, fill = team),
    bound = field_outline, 
    alpha = 0.4, 
    colour = "gray"
  ) +
  geom_delaunay_segment(
    data = play %>% filter(frame == shot_fr, team == "attack"),
    aes(x = x, y = y),
    colour = "white"
  ) +
  geom_point(
    data = play %>% filter(frame == shot_fr),
    aes(x = x, y = y, color = team),
    alpha = 0.9
  )

play_frames <- ggplot() +
  theme_minimal() +
  annotate_pitch() +
  geom_point(
    data = play,
    aes(
      x = x,
      y = y,
      color = team
    )
  ) +
  transition_time(frame) +
  ease_aes("linear") +
  NULL

play_len <- length(unique(play$frame))
play_anim <- animate(
  play_frames,
  fps = 20, 
  nframe = play_len,
  width = 850,
  height = 490
)

anim_save("test.gif", play_anim)











