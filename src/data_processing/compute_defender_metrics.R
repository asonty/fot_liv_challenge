"
FoT Liverpool Challenge
> name: compute_defender_metrics.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - compute defender metrics:
      - compute distance between defenders and ball
      - compute defenders' interception radii
      - compute defenders' interception loci
> dependencies:
    + dplyr
"
compute_defender_metrics <- function(play_data, x_dim = 106, y_dim = 68) {
  print("  computing defender metrics ----")
  
  print("    computing distances from defenders to ball...")
  ball_locs <- play_data %>% 
    filter(player == 0) %>% 
    select(frame, x, y) %>% 
    rename(x_ball = x, y_ball = y)
  
  def_metrics <- play_data %>% 
    filter(team == "defense") %>% 
    select(key, frame, player, player_role, dx, dy, x, y, vx, vy, v, v_theta, vx_norm, vy_norm) %>% 
    left_join(ball_locs, by = "frame") %>% 
    mutate(
      ball_delta = sqrt((x-x_ball)^2 + (y-y_ball)^2)
    ) %>% 
    select(
      !c(x_ball, y_ball)
    )
  
  print("    computing defender incerception radii...")
  def_metrics <- def_metrics %>% 
    mutate(
      side_int_rad = case_when(
        ball_delta > 20 ~ 1.5,
        ball_delta < 1 ~ 0.5,
        TRUE ~ (ball_delta-1) / (20-1) + 0.5
      ),
      fwd_int_rad = v / 4 + side_int_rad
    )
  
  #  double radii for keepers
  def_metrics[def_metrics$player_role == "keeper", ]$side_int_rad <- (
    2 * def_metrics[def_metrics$player_role == "keeper", ]$side_int_rad
  )
  def_metrics[def_metrics$player_role == "keeper", ]$fwd_int_rad <- (
    2 * def_metrics[def_metrics$player_role == "keeper", ]$fwd_int_rad
  )
  
  print("    computing defender incerception loci...")
  def_metrics <- def_metrics %>% 
    mutate(
      lw_x = x + side_int_rad*vy_norm, 
      lw_y = y - side_int_rad*vx_norm,
      rw_x = x - side_int_rad*vy_norm,
      rw_y = y + side_int_rad*vx_norm,
      fwd_x = x + fwd_int_rad*vx_norm,
      fwd_y = y + fwd_int_rad*vy_norm,
      top_x = NaN,
      top_y = NaN,
      bot_x = NaN,
      bot_y = NaN
    ) 
  
  def_metrics <- def_metrics %>% 
    mutate(
      top_y = pmax(lw_y, rw_y, fwd_y),
      bot_y = pmin(lw_y, rw_y, fwd_y)
    )
  
  def_metrics[which(def_metrics$top_y == def_metrics$rw_y), ]$top_x <- (
    def_metrics[which(def_metrics$top_y == def_metrics$rw_y), ]$rw_x
  )
  def_metrics[which(def_metrics$top_y == def_metrics$lw_y), ]$top_x <- (
    def_metrics[which(def_metrics$top_y == def_metrics$lw_y), ]$lw_x
  )
  def_metrics[which(def_metrics$top_y == def_metrics$fwd_y), ]$top_x <- (
    def_metrics[which(def_metrics$top_y == def_metrics$fwd_y), ]$fwd_x
  )
  
  def_metrics[which(def_metrics$bot_y == def_metrics$rw_y), ]$bot_x <- (
    def_metrics[which(def_metrics$bot_y == def_metrics$rw_y), ]$rw_x
  )
  def_metrics[which(def_metrics$bot_y == def_metrics$lw_y), ]$bot_x <- (
    def_metrics[which(def_metrics$bot_y == def_metrics$lw_y), ]$lw_x
  )
  def_metrics[which(def_metrics$bot_y == def_metrics$fwd_y), ]$bot_x <- (
    def_metrics[which(def_metrics$bot_y == def_metrics$fwd_y), ]$fwd_x
  )
  
  print("    done.")
  return(def_metrics)
}