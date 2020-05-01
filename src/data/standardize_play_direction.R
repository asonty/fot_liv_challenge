"
FoT Liverpool Challenge
> name: standardize_play_direction.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - label ball
    - standardize attack direction (to the right)
> dependencies:
    none
"
standardize_play_direction <- function(play_data) {
  print("  standardizing play direction ----")
  
  # * label ball ----
  # play_data[play_data$player == 0, ]$team <- "ball"
  
  # * flip coordinates to attacking right (if necessary) ----
  #  find final position of ball
  #  use this to determine original direction of play
  final_ball_pos <- play_data %>%
    filter(player == 0) %>%
    top_n(-1, desc(frame))
  
  #  flip positions and position deltas if necessary
  if (final_ball_pos[1, ]$x > 90) {
    print("    attack direction ---->")
  } else if (final_ball_pos[1, ]$x < 10) {
    print("    <---- attack direction")
    print("    flipping coordinates...")
    play_data$x <- 100 - play_data$x
    play_data$y <- 100 - play_data$y
    play_data$dx <- -1 * play_data$dx
    play_data$dy <- -1 * play_data$dy
  }
  
  print("    done.")
  return(play_data)
}

print("loaded: standardize_play_direction.R")