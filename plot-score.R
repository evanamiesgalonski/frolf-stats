source("header.R")

sc_file <- "/Users/evanamies-galonski/e/data/frolf-stats/UDisc Scorecards.csv"

sc <- read.csv(sc_file) %>%
  rename_all(to_snake_case) %>%
  filter(player_name %in% c("Lesaucier", "Par")) %>%
  mutate(course_name = case_when(
    course_name == "Dg" ~ "Southern Discomfort",
    course_name == "Mst" ~ "Northern Comfort",
    .default = course_name
      )
    ) %>%
  select(
    player_name, course_name, layout_name, start_date, end_date, round_rating,
    hole_1, hole_2, hole_3, hole_4, hole_5, hole_6, hole_7, hole_8, hole_9, 
    hole_10, hole_11, hole_12, hole_13, hole_14, hole_15, hole_16, hole_17, 
    hole_18
      )

par <- sc %>%
  filter(player_name == "Par") %>%
  pivot_longer(contains("hole"), names_to = "hole", values_to = "par") %>%
  select(course_name, layout_name, hole, par) %>%
  distinct() %>%
  mutate(hole = str_replace(hole, "hole_", ""))

score <- sc %>%
  filter(player_name == "Lesaucier") %>%
  pivot_longer(contains("hole"), names_to = "hole", values_to = "strokes") %>%
  mutate(
    hole = str_replace(hole, "hole_", ""),
    date_time_start = parse_date_time(start_date, orders = "%Y-%m-%d %H%M", tz = "UTC"),
    date_time_start = dtt_adjust_tz(date_time_start, "PST8PDT"),
    date_time_end = parse_date_time(end_date, orders = "%Y-%m-%d %H%M", tz = "UTC"),
    date_time_end = dtt_adjust_tz(date_time_end, "PST8PDT")
    ) %>%
  filter(!is.na(strokes)) %>%
  left_join(par, by = c("course_name", "layout_name", "hole")) %>%
  mutate(score = strokes - par) %>%
  select(course_name, layout_name, date_time_start, date_time_end, round_rating, hole, par, strokes, score)


# quick summary for game stats
# start / end, score, rating
# maybe add the total strokes scored and to par for the course?
game <- score %>%
  group_by(course_name, layout_name, date_time_start, date_time_end, round_rating) %>%
  summarise(
    strokes = sum(strokes),
    par = sum(par),
    score = sum(score), 
    .groups = "drop") %>%
  arrange(date_time_start) %>%
  mutate(game_id = row_number())
 

course_games <- game %>% filter(course_name == "Southern Discomfort")
par_line <- course_games$par %>% only()


# need to see numbers for it to make sense and need to see each stroke as a unit/block
# need to show on scale that ignores the number space of big black bars at bottom.
# just the valence better for that? <-  yes
gp <- ggplot(data = course_games) +
  aes(x = game_id, y = strokes) +
  geom_col(colour = "white", fill = "black") +
  geom_hline(yintercept = par_line, linetype = "dashed", colour = "red", alpha = 1/2)

graphics.off()
sbf_open_window(10, 4)
sbf_print(gp)


  
# more plots of score by hole? x = date, y = score, 
# show floating bar par centered at score with 
# birdies stacking below and bogies above

# plot rating by date?