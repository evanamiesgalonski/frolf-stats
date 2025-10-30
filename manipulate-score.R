source("header.R")

sc_file <- "/Users/evanamies-galonski/e/data/frolf-stats/UDisc Scorecards.csv"

sc <- read.csv(sc_file) %>%
  rename_all(to_snake_case) %>%
  filter(player_name %in% c("Lesaucier", "Par")) %>%
  mutate(
    layout_name = if_else(str_detect(tolower(course_name), "dg"), "Main", layout_name),
    layout_name = case_when(
      str_detect(tolower(course_name), "switch") ~ "Switch",
      str_detect(tolower(course_name), "rev") & !str_detect(tolower(course_name), "18") ~ "Reverse 9",
      str_detect(tolower(course_name), "rev") & str_detect(tolower(course_name), "18") ~ "Reverse 18",
      .default = layout_name
    ),
    course_name = case_when(
      str_detect(tolower(course_name), "dg") ~ "Southern Discomfort",
      course_name == "Mst" ~ "Northern Comfort",
      .default = course_name
    ),
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
  filter(!is.na(par)) %>%
  mutate(
    hole = str_replace(hole, "hole_", ""),
    par = if_else(
      str_detect(tolower(layout_name), "rev|switch") & hole %in% c(5, 8),
      3, par
        )
    )  %>%
  distinct()

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
  left_join(par, by = c("course_name", "layout_name", "hole"))

score %<>% mutate(
    strokes = if_else(dtt_date(date_time_start) <= dtt_date("2025-04-07") & strokes == 2, 3, strokes),
    hole = as.integer(hole),
    score = strokes - par,
    layout_name = case_when(
      str_detect(tolower(layout_name), "rev|switch") & hole %in% 1:9 ~ "Reverse",
      str_detect(tolower(layout_name), "rev|switch") & hole %in% 10:18 ~ "Main",
      .default = layout_name
    )) %>%
  select(
        course_name, layout_name, date_time_start, date_time_end, round_rating,
        hole, par, strokes, score
        )


# every rev 5 before long tee par 3 date should be 3 strokes at best.

score_chk <- score %>%
  filter(str_detect(tolower(layout_name), "rev|switch"), hole == 5)


# quick summary for game stats
# start / end, score, rating
# maybe add the total strokes scored and to par for the course?
game_9s <- score %>%
  filter(strokes != 0) %>%
  mutate(
    nine = if_else(hole <= 9 , "1front", "2back"),
    ) %>%
  group_by(course_name, layout_name, nine, date_time_start, date_time_end, round_rating) %>%
  summarise(
    holes = n(),
    strokes = sum(strokes),
    par = sum(par),
    score = sum(score),
    .groups = "drop") %>%
  arrange(date_time_start) %>%
  mutate(`9_id` = row_number())

# SOMETHING MAJORLY WRONG WITH COURSE GROUPING
# SEE PLOT  3IN PLOT SCORE MAIN 

game <- game_9s %>%
  group_by(course_name, layout_name, date_time_start, date_time_end, round_rating) %>%
  summarise(
    holes = n(),
    strokes = sum(strokes),
    par = sum(par),
    score = sum(score), 
    .groups = "drop") %>%
  arrange(date_time_start) %>%
  mutate(game_id = row_number())

rm(score_chk)
sbf_set_sub("score", rm = TRUE, ask = FALSE)
sbf_save_datas()
  

