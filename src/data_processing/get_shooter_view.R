"
FoT Liverpool Challenge
> name: get_shooter_view.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - get shooter's view on goal in plotable df
> dependencies:
    + dplyr
    + tidyr
"
get_shooter_view <- function(play_data, x_dim = 106, y_dim = 68) {
  print("  getting shooter view ----")
  
  goal_width <- 7.32
  
  print("    creating shooter view df...")
  shooter_view <- play_data %>% 
    select(frame, x, y, player_role) %>% 
    filter(player_role == "shooter") %>%
    group_by(frame) %>% 
    do(
      rbind(
        ., 
        data.frame(
          frame = c(NA, NA),
          x = c(x_dim, 
                # x_dim+2, 
                # x_dim+2, 
                x_dim),
          y = c(y_dim/2 + goal_width/2, 
                # y_dim/2 + goal_width/2,
                # y_dim/2 - goal_width/2,
                y_dim/2 - goal_width/2),
          player_role = rep("shooter", 2)
        )
      )
    ) %>% 
    ungroup() %>% 
    data.frame() %>% 
    tidyr::fill(frame)
  
  print("    done.")
  return(shooter_view)
}