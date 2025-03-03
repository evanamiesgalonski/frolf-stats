source("header.R")

disc <- tribble(
  ~disc, ~speed, ~glide, ~turn, ~fade, ~have,
  "stalker", 7, 5, -1, 2, T, 
  "undertaker", 9, 5, -1, 2, T,
  "sting", 7, 5, -2, 1, T,
  "explorer", 7, 7, 0, 2, T,
  "buzz os", 5, 5, 0, 3, T,
  "pathfinder", 5, 5, 0, 1, T,
  "nebula", 5, 4, -0.5, 2, T,
  "tempo", 4, 4, 0, 2.5, T,
  "swarm", 5, 3, 0, 4, T
) %>%
  mutate(inc_origin = 0, inc_turn = 2, inc_fade = 1) %>%
  pivot_longer(
    c(inc_origin, inc_turn, inc_fade), 
    names_to = NULL, values_to = "y_increment", names_prefix = "disc"
    ) %>%
  mutate(
    y_increment = if_else(y_increment == 0, 0, y_increment * speed)
    ) %>%
  group_by(disc) %>%
  mutate(
    y = cumsum(y_increment),
    x = case_when(
      row_number() == 1 ~ 0,
      row_number() == 2 ~ turn,
      row_number() == 3 ~ fade,
      )
    ) %>%
  select(-y_increment) %>%
  ungroup()

disc %<>%
  filter(speed >= 7)

# plotting of 0, 1, 2 on y, by 0, turn, fade on x  
# increment for y is expanded by speed of disc.
gp <- ggplot(data = disc) +
  aes(x = x, y = y, colour = disc) +
  geom_path() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", alpha = 0.5) +
  expand_limits(x = c(-5, 5)) +
  scale_colour_disc_poisson(order = c(2:9)) +
  ggdark::dark_theme_classic()
  
graphics.off()
sbf_open_window(10, 10)
sbf_print(gp)



  