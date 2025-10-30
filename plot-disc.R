source("header.R")

turn_activation <- 4

fade_offset <- 0.1

# need a param that effects end point, inhibits turn,
# but does not pull mid point away from turn
# just add one elbow thats like pre turn thats straiter

extra_cols <- poispalette:::pois_cols()[2:9]

disc <- tribble(
  ~disc, ~speed, ~glide, ~turn, ~fade, ~have, ~colour,
  "stalker", 7, 5, -1, 2, T, "#0078FF",
  "undertaker", 9, 5, -1, 2, T, "#9CEDFF",
  "sting", 7, 5, -2, 1, T, "#7713FF",
  "explorer", 7, 7, 0, 2, T, "#DBFF00",
  "buzz os", 5, 5, 0, 3, T, "#FF009E",
  "pathfinder", 5, 5, 0, 1, T, "#FF3D20",
  "nebula", 5, 4, -0.5, 2, T, "#FFB319",
  "tempo", 4, 4, 0, 2.5, T, "#FFD3C1",
  "swarm", 5, 3, 0, 4, T, "#FF9CF3",
  "Trail", 10, 5, -1, 1, F, NA,
  "Trance", 8, 5, -2, 1, F, NA,
  "Virus", 9, 5, -3.5, 1, F, NA,
  "Insanity/Lift", 9, 5, -2, 1.5, F, NA,
  "sapphire", 10, 6, -2, 1.5, F, NA,
  "mantra", 9, 6, -2, 1, F, NA
) %>%
  mutate(
    origin = 0, mid = turn_activation, end = 1,
    col_group = is.na(colour)
    ) %>%
  group_by(col_group) %>%
  mutate(colour = if_else(is.na(colour), extra_cols[row_number()], colour)) %>%
  ungroup() %>%
  pivot_longer(
    c(origin, mid, end), 
    names_to = "inc_type", values_to = "y_increment", names_prefix = "disc"
    ) %>%
  mutate(
    y_increment = if_else(y_increment == 0, 0, (y_increment * speed) + (glide * speed / 30))
    ) %>%
  group_by(disc) %>%
  mutate(
    y = cumsum(y_increment),
    x = case_when(
      inc_type == "origin" ~ 0,
      inc_type == "mid" ~ turn +(fade * fade_offset),
      inc_type == "end" ~ fade + turn,
      )
    ) %>%
  select(-y_increment, -col_group) %>%
  ungroup()

splinate <- function(x, y, n = 100, method = "natural")
{
  t <- seq_along(x)
  new_t <- seq(min(t), max(t), length.out = n)
  new_x <- spline(t, x, xout = new_t, method = method)$y
  new_y <- spline(t, y, xout = new_t, method = method)$y
  data.frame(t = new_t, x = new_x, y = new_y)
}

disc_splines <- map_dfr(unique(disc$disc), function(disc_name) {

  disc_dat <- disc %>% filter(disc == disc_name)
  
  splines <- splinate(disc_dat$x, disc_dat$y)
  
  disc_dat %>% slice(1) %>% select(-c(x, y, inc_type)) %>%
    cbind(splines)
})


# disc_splines %<>% filter(speed >= 7)
colors <- disc_splines %>% arrange(disc) %>% pull(colour) %>% unique()

# plotting of 0, 1, 2 on y, by 0, turn, fade on x  
# increment for y is expanded by speed of disc.
gp <- ggplot(data = disc_splines) +
  aes(x = x, y = y, colour = disc) +
  geom_path(linewidth = 2, lineend = "round", alpha = 2/3) +
  # geom_smooth(orientation = "y", level = .9) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey", alpha = 0.3, linewidth = 2) +
  expand_limits(x = c(-5, 5)) +
  labs(x = "turn/position", y = "distance") +
  scale_x_continuous(breaks = c(-3:0)) +
  scale_colour_manual(values = colors) +
  ggdark::dark_theme_classic() +
  # scale_x_reverse(breaks = c(-3:0)) +
  NULL

graphics.off()
sbf_open_window(8)
sbf_print(gp)

