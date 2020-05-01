"
FoT Liverpool Challenge
> name: specify_player_roles.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - specify player roles (keeper, passer(s), shooter, off-ball)
> dependencies:
    + dplyr
"
specify_player_roles <- function(play_data) {
  print("  specifying player roles ----")
  
  # * specify player roles ----
  play_data$player_role <- NA_character_
  
  #  initially, label all players as off-ball
  play_data[play_data$player != 0, ]$player_role <- "off-ball"
  
  #  identify passer(s)
  #    find player_id for all players who passed the ball
  passers <- play_data %>%
    filter(player_action == "pass") %>%
    select(player)
  play_data[which(play_data$player %in% passers$player), ]$player_role <- "passer"
  
  #  identify shooter
  #    find player_id for player who shot the ball
  shooter <- play_data %>%
    filter(player_action == "shot") %>%
    select(player)
  play_data[which(play_data$player %in% shooter$player), ]$player_role <- "shooter"
  
  #  identify keeper
  #    find player_id for right-most defensive player on field at frame 0
  keeper <- play_data %>%
    filter(frame == 0 & team == "defense") %>%
    top_n(-1, desc(x)) %>%
    select(player)
  play_data[which(play_data$player %in% keeper$player), ]$player_role <- "keeper"
  
  print("    done.")
  return(play_data)
}

print("loaded: specify_player_roles.R")