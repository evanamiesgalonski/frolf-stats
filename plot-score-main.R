source("header.R")

sbf_set_sub("score")
sbf_load_datas()

# score hist, compare dg to other rated
main_score <- game %>% filter(
  course_name %in% c(
    "Art Gibbon Park", "Marsh Creek", "All Yew Need Disc Golf Course",
    "Ymir Whirl Disc Golf Course", "Southern Discomfort", "Balfour Disc Golf Course")
) %>%
  mutate(course_layout = paste(course_name, layout_name)) %>%
  group_by(course_layout) %>%
  mutate(mean_score = mean(score), diff_from_mean = abs(mean_score - score))


message("plotting recent - 6 mo average")

main_score_recent <- main_score %>% filter(date_time_start > dtt_subtract_months(dtt_date_time(Sys.Date()), 6)) %>%
  group_by(course_layout) %>%
  mutate(
    mean_score = mean(score), diff_from_mean = abs(mean_score - score),
    mean_rating = round(mean(round_rating, na.rm = TRUE)), diff_rating_from_mean = abs(mean_rating - round_rating)
  )

gp <- ggplot(data = main_score_recent) +
  facet_wrap(~ course_layout, scales = "free_x") + 
  geom_histogram(aes(x = score), colour = "black", binwidth = 1) +
  geom_vline(aes(xintercept = mean_score), colour = "red", alpha = 0.5) +
  dark_theme_gray() +
  scale_fill_grad_poisson() +
  scale_x_binned(breaks = min(main_score$score):max(main_score$score))

sbf_open_window(10, 6)
sbf_print(gp)

# rating
gp <- ggplot(data = main_score_recent %>% filter(!is.na(mean_rating))) +
  facet_wrap(~ course_layout) + 
  geom_histogram(aes(x = round_rating), colour = "black", binwidth = 20) +
  geom_vline(aes(xintercept = mean_rating), colour = "red", alpha = 0.5) +
  dark_theme_gray() +
  scale_fill_grad_poisson() +
  scale_x_binned(breaks = seq(120, 220, by = 5))

sbf_open_window(10, 6)
sbf_print(gp)

# next: take samples of date ranges and capture rating and score then plot
# against each other. slope may allow u to infer rating for DG course

main_rating_score <- main_score %>%
  group_by(course_layout) %>%
  mutate(games = n()) %>%
  filter(
    games > 10,
    date_time_start > dtt_date_time("2025-01-01")
  ) %>%
  summarise(mean_score = mean(score), mean_rating = mean(round_rating))


dat <- data.frame(line = c(160, 170, 180, 190, 200))

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

gp <- ggplot(data = rating) +
  aes(x = seq, y = rating) +
  geom_smooth(method = "lm", formula = 'y ~ x') + 
  geom_point(aes(colour = months_exp), size = 0.7) +
  geom_hline(data = dat, aes(yintercept = line), alpha = 0.25, linetype = "dashed", linewidth = 0.25) +
  geom_hline(aes(yintercept = 180), alpha = 0.5, linewidth = 0.25, linetype = "dashed", colour = "red") +
  dark_theme_gray() +
  scale_y_continuous(breaks = 12:22 * 10) +
  scale_colour_viridis()

sbf_open_window(10, 5)
sbf_print(gp)

gp <- ggplot(
  data = rating %>% filter(
    main_course,
    # start_date <= dtt_date("2025-03-10")
  )
) +
  facet_wrap(~course_name, scales = "free_x") +
  aes(x = seq_course, y = rating) +
  geom_smooth(method = "lm", formula = 'y ~ x') + 
  geom_point(aes(colour = months_exp), size = 0.7) +
  geom_hline(data = dat, aes(yintercept = line), alpha = 0.25, linetype = "dashed", linewidth = 0.25) +
  geom_hline(aes(yintercept = 180), alpha = 0.5, linewidth = 0.25, linetype = "dashed", colour = "red") +
  dark_theme_gray() +
  scale_y_continuous(breaks = 12:22 * 10) +
  scale_colour_viridis()

sbf_open_window(14, 10)
sbf_print(gp)
