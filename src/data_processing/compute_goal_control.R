"
FoT Liverpool Challenge
> name: compute_goal_control.R
> author: Adam Sonty (@sonty_1)
> functionality:
    - compute shooter's goal control at each frame
> dependencies:
    + dplyr
"
compute_goal_control <- function(play_data, obstruct_poly, x_dim = 106, y_dim = 68) {
  print("  computing shooter goal control ----")
  
  goal_width <- 7.32
  
  print("    computing goal control at each frame...")
  def_gc_ranges <- obstruct_poly %>% 
    select(frame, bot_y_proj, top_y_proj) %>% 
    rename(start = bot_y_proj, stop = top_y_proj)
  
  def_gc_ranges_split <- def_gc_ranges %>% 
    group_split(frame)
  
  def_gc_ranges_per_frame <- NULL
  for (i in 1:length(def_gc_ranges_split)) {
    this_frame_def_gc <- def_gc_ranges_split[[i]] %>% 
      arrange(start) %>% 
      group_by(g = cumsum(cummax(lag(stop, default = first(stop))) < start)) %>% 
      summarise(
        start = first(start),
        stop = max(stop),
        len = stop - start)
    
    this_frame_def_gc <- this_frame_def_gc %>% 
      mutate(
        frame = def_gc_ranges_split[[i]]$frame[1]
      ) %>% 
      select(frame, start, stop, len)
    def_gc_ranges_per_frame <- bind_rows(def_gc_ranges_per_frame, this_frame_def_gc)
  }
  
  shooter_gc <- def_gc_ranges_per_frame %>% 
    group_by(frame) %>% 
    summarise(
      defense_gc_dist = sum(len)
    ) %>% 
    mutate(
      attack_gc_dist =  goal_width - defense_gc_dist,
      attack_gc = attack_gc_dist / goal_width
    )
  
  shooter_gc <- play_data %>% 
    select(frame) %>%  
    distinct() %>% 
    left_join(shooter_gc, by = "frame")
  
  shooter_gc[is.na(shooter_gc$defense_gc_dist), ]$defense_gc_dist <- 0
  shooter_gc[is.na(shooter_gc$attack_gc), ]$attack_gc_dist <- goal_width
  shooter_gc[is.na(shooter_gc$attack_gc), ]$attack_gc <- 1

  print("    done.")
  return(shooter_gc)
}