"
FoT Liverpool Challenge
> name: rescale_coordinates.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - rescales coordinates to pitch size (106m x 68m) specified by 
      Laurie Shaw in FoT lecture 4
      <https://youtu.be/8TrleFklEsE?t=632>
      this allows us to use our tracking data to calculate meaningful
      values involving distances (e.g. velocity in m/s)
> dependencies:
    none
"
rescale_coordinates <- function(play_data, x_dim = 106, y_dim = 68) {
  print("  rescaling coordinates ----")
  
  play_data$x <- play_data$x / 100 * x_dim
  play_data$dx <- play_data$dx / 100 * x_dim
  
  play_data$y <- play_data$y / 100 * y_dim
  play_data$dy <- play_data$dy / 100 * y_dim
  
  print("    done.")
  return(play_data)
}