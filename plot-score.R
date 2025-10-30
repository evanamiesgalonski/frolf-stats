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
  filter(strokes != 0) %>%
  group_by(course_name, layout_name, date_time_start, date_time_end, round_rating) %>%
  summarise(
    holes = n(),
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
  geom_hline(yintercept = par_line, linetype = "dashed", colour = "red", alpha = 1/2) +
  coord_cartesian(ylim = c(min(course_games$strokes) -5, NA)) +
  dark_theme_gray()

graphics.off()
sbf_open_window(6, 2)
sbf_print(gp)


gp <- ggplot(data = course_games) +
  aes(x = score) +
  geom_histogram(binwidth = 1, colour = "black") +
  dark_theme_gray() +
  scale_y_continuous(breaks = 1:12) + 
  geom_vline(xintercept = mean(course_games$score), colour = "red") +
  # scale_x_binned(breaks = -1:-13) +
  NULL

graphics.off()
sbf_open_window()
sbf_print(gp)


hole_score <- score %>%
  filter(course_name == "Southern Discomfort") %>%
  select(date_time_start, hole, score) %>%
  mutate(score = factor(score), hole = factor(hole)) %>%
  pivot_wider(names_from = hole, values_from = score) %>%
  select(-date_time_start)


course_score <- score %>%
  filter(course_name == "Southern Discomfort") %>%
  group_by(hole) %>%
  mutate(hole_mean = mean(score), hole = as.integer(hole))

gp <- ggplot(data = course_score) +
  aes(x = hole, y = hole_mean) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  dark_theme_gray() +
  scale_x_continuous(breaks = 1:18)

sbf_open_window(7, 3)
sbf_print(gp)


gp <- ggplot(data = course_score) +
  # facet_wrap(~ hole) +
  geom_col(aes(x = hole, y = score, colour = score, fill = score))+
  scale_colour_grad_poisson() +
  scale_fill_grad_poisson() +
  scale_x_continuous(breaks = 1:18) +
  dark_theme_gray()

graphics.off()
sbf_open_window(6)
sbf_print(gp)



# above but colour by game continuous


# more plots of score by hole? x = date, y = score, 
# show floating bar par centered at score with 
# birdies stacking below and bogies above

# plot rating by date?

# rating/score relationship

rating <- game %>% filter(!is.na(round_rating)) %>%
  mutate(course_layout = paste(course_name, layout_name, sep = "-")) %>%
  filter(
    course_name %in% c(
      "Marsh Creek", "Art Gibbon Park", "Ymir Whirl Disc Golf Course",
      "All Yew Need Disc Golf Course"
      )
    ) %>%
  mutate(
    ten_floor = floor(round_rating / 10),
    ten_floor = ten_floor * 10,
         )

# proportion of rounds above x rating
rating %<>% mutate(ten_floor = if_else(ten_floor < 160, NA_real_, ten_floor))
sum(!is.na(rating$ten_floor)) / nrow(rating)

dat <- data.frame(line = c(160, 170, 180, 190, 200))

gp <- ggplot(rating) +
  aes(x = score, y = round_rating) +
  facet_wrap(~ course_layout) +
  geom_point(alpha = 0.5, size = 4, aes(colour = ten_floor)) +
  dark_theme_gray() +
  geom_hline(data = dat, aes(yintercept = line, colour = line), alpha = 0.75, linetype = "dashed") +
  scale_colour_viridis() +
  scale_x_reverse(breaks = c(min(rating$score):max(rating$score))) +
  scale_y_continuous(breaks = c(10:22) * 10) + 
  labs(x = "Score", y = "Rating", colour = "rating") 

graphics.off()
sbf_open_window(10, 8)
sbf_print(gp)


# rating


rating <- sc %>%
  filter(
    player_name == "Lesaucier",
    !is.na(round_rating)
    ) %>%
  group_by(start_date, course_name, layout_name) %>%
  summarise(rating = only(round_rating), .groups = "drop") %>%
  mutate(
    start_date = dtt_date_time(start_date),
    pdga_apprx = (rating * 2) + 500,
    main_course = if_else(
      course_name %in% c(
        "Art Gibbon Park", "Marsh Creek", "All Yew Need Disc Golf Course",
        "Ymir Whirl Disc Golf Course"
        ), TRUE, FALSE
      )
    ) %>%
  arrange(start_date) %>%
  mutate(
    months_exp = floor(dtt_diff(start_date, first(start_date), units = "days") / 30),
    seq = row_number()
    ) %>%
  group_by(course_name) %>%
  mutate(seq_course = row_number())
