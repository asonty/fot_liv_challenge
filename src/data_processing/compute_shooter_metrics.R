"
FoT Liverpool Challenge
> name: compute_shooter_metrics.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - compute shooter's metrics:
      - angle
      - distance from center of goal
      - effective goal width
> dependencies:
    + dplyr
"
compute_shooter_metrics <- function(play_data, x_dim = 106, y_dim = 68) {
  print("  computing shooter metrics ----")
  
  goal_width <- 7.32
  goal_cent_x <- x_dim
  goal_cent_y <- y_dim/2
  
  shooter_metrics <- play_data %>% 
    filter(player_role == "shooter") %>% 
    select(key, frame, player, player_num, player_role, player_action, x, y)
  
  shooter_metrics$goal_dist <- NaN
  shooter_metrics$goal_theta <- NaN
  shooter_metrics$goal_theta_deg <- NaN
  shooter_metrics$eff_goal_width <- NaN
  
  print("    computing distance from shooter to center of goal...")
  shooter_metrics$goal_dist <- sqrt(
    (goal_cent_x - shooter_metrics$x)^2 +
      (goal_cent_y - shooter_metrics$y)^2
  )
  
  print("    computing shooter's goal angle...")
  top_goal_dist <- sqrt(
    (goal_cent_x - shooter_metrics$x)^2 +
      (goal_cent_y + goal_width/2 - shooter_metrics$y)^2
  )
  
  bot_goal_dist <- sqrt(
    (goal_cent_x - shooter_metrics$x)^2 +
      (goal_cent_y - goal_width/2 - shooter_metrics$y)^2
  )
  
  top_goal_x_comp <- (
    (goal_cent_x - shooter_metrics$x)
  )
  
  top_goal_y_comp <- (
    (goal_cent_y + goal_width/2 - shooter_metrics$y)
  ) 
  
  bot_goal_x_comp <- (
    (goal_cent_x - shooter_metrics$x)
  )
  
  bot_goal_y_comp <- (
    (goal_cent_y - goal_width/2 - shooter_metrics$y)
  ) 
  
  dot_prod <- (
    top_goal_x_comp * bot_goal_x_comp +
      top_goal_y_comp * bot_goal_y_comp
  )
  
  shooter_metrics$goal_theta <- acos(
    dot_prod / (top_goal_dist * bot_goal_dist)
  )
  
  shooter_metrics$goal_theta_deg <- shooter_metrics$goal_theta * 180/pi
  
  print("    computing shooter's effective goal width")
  shorter_side <- pmin(top_goal_dist, bot_goal_dist)
  
  shooter_metrics$eff_goal_width <- (
    2 * shorter_side * sin(0.5 * shooter_metrics$goal_theta)
  )
  
  print("    done.")
  return(shooter_metrics)
}

