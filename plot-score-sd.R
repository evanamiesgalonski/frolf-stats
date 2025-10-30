source("header.R")

sbf_set_sub("score")
sbf_load_datas()

hole_score <- score %>%
  filter(course_name == "Southern Discomfort", layout_name == "Main") %>%
  select(date_time_start, hole, score) %>%
  mutate(score = factor(score), hole = factor(hole)) %>%
  pivot_wider(names_from = hole, values_from = score) %>%
  select(-date_time_start)

course_score <- score %>%
  filter(course_name == "Southern Discomfort", layout_name == "Main") %>%
  group_by(hole) %>%
  mutate(
    hole_mean = mean(score), hole = as.integer(hole),
    birdie = if_else(strokes - par == -1, TRUE, FALSE),
    bogie = if_else(strokes - par == 1, TRUE, FALSE)
  )

# above but colour by game continuous
# more plots of score by hole? x = date, y = score, 
# show floating bar par centered at score with 
# birdies stacking below and bogies above

bird_bog <- course_score %>%
  group_by(hole) %>%
  summarise(birdies = sum(birdie),
            bogies = sum(bogie))

gp <- ggplot(data = bird_bog) +
  geom_col(aes(x = hole, y = birdies), colour = "#0087ED", fill = "#0087ED")+
  scale_colour_grad_poisson() +
  scale_fill_grad_poisson() +
  scale_x_continuous(breaks = 1:18) +
  # dark_theme_gray() +
  NULL

sbf_open_window()
sbf_print(gp)

gp <- ggplot(data = bird_bog) +
  geom_col(aes(x = hole, y = bogies), colour = "#CF4F3C", fill = "#CF4F3C")+
  scale_colour_grad_poisson() +
  scale_fill_grad_poisson() +
  scale_x_continuous(breaks = 1:18) +
  # dark_theme_gray() +
  scale_y_reverse() 

sbf_open_window()
sbf_print(gp)