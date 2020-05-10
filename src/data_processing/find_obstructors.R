"
FoT Liverpool Challenge
> name: find_obstructors.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - find players in shooter's goal-view
    - compute defenders' obstruction projection
> dependencies:
    + dplyr
    + purrr
    + sp
    + tidyr
"
find_obstructors <- function(play_data, def_metrics, x_dim = 106, y_dim = 68) {
  print("  finding obstructors ----")
  
  goal_width <- 7.32
  
  print("    finding obstructors...")
  shooter_lg_poly <- play_data %>% 
    filter(player_role == "shooter") %>%
    select(frame, x, y) %>% 
    rename(x1 = x, y1 = y) %>% 
    mutate(
      x2 = x_dim,
      y2 = y_dim/2 + goal_width/2 + 1.5,
      x3 = x_dim,
      y3 = y_dim/2 - goal_width/2 - 1.5
    )
  
  #  find which defenders were in the shooter's view on goal + 1.5 m out
  def_in_shooter_lg_poly <- play_data %>% 
    filter(player != 0 & team == "defense") %>% 
    select(key, player, x, y, frame) %>% 
    left_join(shooter_lg_poly, by = c("frame"), how = "full") %>% 
    transmute(
      key, player, frame,
      x, y,
      polyx = purrr::pmap(list(x1, x2, x3),list),
      polyy = purrr::pmap(list(y1, y2, y3),list),
      in_poly = purrr::pmap(list(x, y, polyx, polyy), sp:::point.in.polygon)
    ) %>% 
    filter(in_poly == 1) %>% 
    select(key) %>% 
    left_join(def_metrics, by = "key")
  
  #  deftermine obstruction projections
  obstruct_poly <- def_in_shooter_lg_poly %>% 
    left_join(
      play_data %>% 
        filter(player_role == "shooter") %>% 
        select(frame, x, y) %>% 
        rename(x_sht = x, y_sht = y), 
      by = "frame") %>% 
    mutate(
      top_x_proj = x_dim,
      top_y_proj = (y_sht-top_y)/(x_sht-top_x) * (x_dim - top_x) + top_y,
      bot_x_proj = x_dim,
      bot_y_proj = (y_sht-bot_y)/(x_sht-bot_x) * (x_dim - bot_x) + bot_y
    ) %>% 
    select(key, frame, player, player_role, x, y, side_int_rad, fwd_int_rad,
           top_x, top_y, top_x_proj, top_y_proj,
           bot_x, bot_y, bot_x_proj, bot_y_proj) %>% 
    tidyr::drop_na()
  
  #  reduce obstructors set
  obstruct_poly <- subset(obstruct_poly, !(bot_y_proj > y_dim/2 + goal_width/2))
  obstruct_poly <- subset(obstruct_poly, !(top_y_proj < y_dim/2 - goal_width/2))
  
  #  reduce projections outside of the goal width
  # obstruct_poly[
  #   obstruct_poly$top_y_proj > y_dim/2 + goal_width/2, 
  #   ]$top_y <- obstruct_poly[
  #     obstruct_poly$top_y_proj > y_dim/2 + goal_width/2, 
  #     ]$y
  obstruct_poly[
    obstruct_poly$top_y_proj > y_dim/2 + goal_width/2, 
    ]$top_y_proj <- y_dim/2 + goal_width/2
  
  # obstruct_poly[
  #   obstruct_poly$bot_y_proj < y_dim/2 - goal_width/2, 
  #   ]$bot_y <- obstruct_poly[
  #     obstruct_poly$bot_y_proj < y_dim/2 - goal_width/2, 
  #     ]$y
  obstruct_poly[
    obstruct_poly$bot_y_proj < y_dim/2 - goal_width/2, 
    ]$bot_y_proj <- y_dim/2 - goal_width/2
  
  print("    done.")
  return(obstruct_poly)
}
