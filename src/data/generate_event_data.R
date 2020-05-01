"
FoT Liverpool Challenge
> name: generate_event_data.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - identify frames in which players make contact with the ball
    - label event frames
> dependencies:
    + dplyr
    + janitor
"
generate_event_data <- function(play_data) {
  print("  generating event data ----")
  
  # * identify frames in which players make contact with the ball
  print("    identifying frames in which players contact ball...")
  #  set all players to never have ball
  play_data$player_has_ball <- FALSE
  
  #  get all frames in which the ball and a player have the same coordinates
  #  i.e. a player possesses the ball
  #  drop rows of ball data, arrange by frame
  player_contact_ball <- play_data %>% 
    janitor::get_dupes(x, y, frame) %>%
    tidyr::drop_na(player_num) %>%
    arrange(frame)
  
  #  label all frames for players who have the ball
  play_data[which(play_data$key %in% player_contact_ball$key), ]$player_has_ball <- TRUE
  
  # * generate event data ---
  print("    labeling event frames...")
  #  create player_event_id 
  #  (this helps differentiate between the same player having the ball multiple times)
  #    use frame diff between row and row above to create event identifiers
  player_contact_ball$event_id <- c(0, diff(as.matrix(player_contact_ball$frame)))
  #    wherever the diff == 1, that indicates a player is carrying the ball
  #    wherever the diff != 1, that indicates a different ball event
  #    we will use values != 1 to label the entirety of an event, 
  #    so replace 1's with those values
  player_contact_ball[player_contact_ball$event_id == 1, ]$event_id <- NA
  player_contact_ball <- player_contact_ball %>% 
    tidyr::fill(event_id)
  #    finally, crate unique player_event_id by concat player_id and event_id
  #    we now have a unique identifier for the entirety of each player-ball event
  player_contact_ball$player_event_id <- paste(
    player_contact_ball$player, 
    player_contact_ball$event_id,
    sep = "-")
  
  #  initially, label all player_event frames as carries
  play_data$player_action <- NA_character_
  play_data[which(play_data$key %in% player_contact_ball$key), ]$player_action <- "carry"
  
  #  label first frame in each player_event as reception
  ball_received <- player_contact_ball %>% 
    group_by(player_event_id) %>% 
    top_n(1, desc(frame))
  play_data[which(play_data$key %in% ball_received$key), ]$player_action <- "reception"
  
  #  label final frame in each player_event as pass
  ball_passed <- player_contact_ball %>% 
    group_by(player_event_id) %>% 
    top_n(-1, desc(frame))
  play_data[which(play_data$key %in% ball_passed$key), ]$player_action <- "pass"
  
  #  label final player_event frame (out of all player_event frames) as shot
  ball_shot <- player_contact_ball %>%
    top_n(-1, desc(frame))
  play_data[which(play_data$key %in% ball_shot$key), ]$player_action <- "shot"
  
  print("    done.")
  return(play_data)
}

print("loaded: generate_event_data.R")