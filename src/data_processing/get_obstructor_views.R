"
FoT Liverpool Challenge
> name: get_obstructor_views.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - get obstructors' views in plotable df
> dependencies:
    + none
"
get_obstructor_views <- function(obstruct_poly, x_dim = 106, y_dim = 68) {
  print("  getting obstructor views ----")
  
  goal_width <- 7.32
  
  print("    creating obstructors' views df...")
  obstruct_views <- NULL
  for (i in 1:nrow(obstruct_poly)) {
    row <- (obstruct_poly[i,])
    frame <- rep(row$frame, 4)
    player <- rep(row$player, 4)
    x <- c(row$top_x, row$top_x_proj, row$bot_x_proj, row$bot_x)
    y <- c(row$top_y, row$top_y_proj, row$bot_y_proj, row$bot_y)
    obstruct_views <- bind_rows(obstruct_views, tibble(frame, player, x, y))
  }
  
  print("    done.")
  return(obstruct_views)
}