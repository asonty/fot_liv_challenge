"
FoT Liverpool Challenge
> name: compute_velocities.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - computes instantaneous velocities (m/s) for all objects on the pitch
> dependencies:
    dplyr
    signal
"
rescale_coordinates <- function(play_data, fps = 20, smooth = TRUE) {
  print("  computing velocities ----")
  
  play_data$vx <- play_data$dx / (1/fps)
  play_data$vy <- play_data$dy / (1/fps)
  play_data$v <- sqrt(play_data$vx^2 + play_data$vy^2)
  
  print("    done.")
  return(play_data)
}