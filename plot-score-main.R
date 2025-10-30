source("header.R")

sbf_set_sub("score")
sbf_load_datas()

dat <- data.frame(line = c(160, 170, 180, 190, 200))

main <- c(
  "Art Gibbon Park", "Marsh Creek", "All Yew Need Disc Golf Course",
  "Ymir Whirl Disc Golf Course", "Highwater", "Balfour Disc Golf Course"
  )

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
    main_course = course_name %in% main,
    main_name = if_else(main_course, course_name, "Other"),
    month_num = dtt_month(start_date)
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
  geom_point(aes(colour = main_name), size = 1.5, alpha = 0.8) +
  geom_hline(data = dat, aes(yintercept = line), alpha = 0.25, linetype = "dashed", linewidth = 0.25) +
  geom_hline(aes(yintercept = 180), alpha = 0.5, linewidth = 0.25, linetype = "dashed", colour = "red") +
  # geom_point(aes(x = seq, y = 120, colour = month_num)) +
  # dark_theme_gray() +
  scale_y_continuous(breaks = 12:24 * 10) +
  scale_colour_disc_poisson() +
  NULL

sbf_open_window(10, 5)
sbf_print(gp)

gp <- ggplot(data = rating %>% filter(main_course)) +
  facet_wrap(~course_name, scales = "free_x") +
  aes(x = seq_course, y = rating) +
  geom_smooth(method = "lm", formula = 'y ~ x') + 
  geom_point(aes(colour = months_exp), size = 0.7) +
  geom_hline(data = dat, aes(yintercept = line), alpha = 0.25, linetype = "dashed", linewidth = 0.25) +
  geom_hline(aes(yintercept = 180), alpha = 0.5, linewidth = 0.25, linetype = "dashed", colour = "red") +
  # dark_theme_gray() +
  scale_y_continuous(breaks = 12:22 * 10) +
  scale_colour_viridis()

sbf_open_window(14, 8)
sbf_print(gp)

game %<>%
  filter(
    !is.na(round_rating), 
    course_name %in% main, 
    !str_detect(tolower(layout_name), "front|back")
  ) %>%
  mutate(course_layout = paste(course_name, layout_name))

# plot score rev by rating and look for exponential
gp <- ggplot(data = game) +
  facet_wrap(~course_layout, scales = "free_x") +
  aes(x = score, y = round_rating, colour = game_id) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') + 
  scale_x_reverse(breaks = min(game$score):max(game$score)) +
  scale_colour_viridis()

sbf_open_window(12, 7)
sbf_print(gp)

# do jags model with regression to estimate rating increase per stroke

# take some groupings of games with the same score, then plot their rating over time
# just show the differece from 0

main_2 <- unique(game$course_layout)
main_2

i <- 1
main_2[i]

rating_shift <- game %>% 
  filter(course_layout == main_2[i]) %>%
  group_by(course_layout, score) %>%
  mutate(n = n()) %>%
  filter(n >= 5)


gp <- ggplot(data = rating_shift) + 
  facet_wrap(~score) +
  aes(x = game_id, y = round_rating) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') 

sbf_open_window(12, 6)
sbf_print(gp)






