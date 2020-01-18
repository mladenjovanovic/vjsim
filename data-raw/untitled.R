

# --------------------------
require(tidyverse)

state <- expand.grid(
  current_time = c(0.1, 0.1125, 0.125, 0.15, 0.2, 0.3),
  current_distance = seq(0, 0.395, length.out = 1000),
  current_velocity = 0)

fgen_data <- fgen_get_output(
  max_force = 3000,
  mass = 75,
  current_time = state$current_time,
  current_distance = state$current_distance,
  current_velocity = state$current_velocity
) %>%
  gather("variable", "value", -(1:5)) %>%
  mutate(
    variable = factor(
    variable,
    levels = c(
      "max_force",
      "force_percentage",
      "force_potential",
      "force_generated",
      "force_viscous",
      "force_total",
      "force_propulsive",
      "acceleration")
    ),
    current_time = factor(current_time)
    )

gg <- ggplot(fgen_data, aes(x = current_distance, y = value, color = current_time, group = current_time)) +
  theme_minimal()+
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL) +
  scale_color_grey(start = 0.8, end = 0.2) +
  ggtitle("Force Generator output at different distance at different times, assuming zero velocity (BW=75kg)")

gg

# -----
state <- expand.grid(
  current_time = seq(0, 0.25, length.out = 1000),
  current_distance = c(0.1, 0.15, 0.2, 0.25, 0.3),
  current_velocity = 0)

fgen_data <- fgen_get_output(
  max_force = 3000,
  mass = 75,
  current_time = state$current_time,
  current_distance = state$current_distance,
  current_velocity = state$current_velocity
) %>%
  gather("variable", "value", -(1:5)) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "max_force",
        "force_percentage",
        "force_potential",
        "force_generated",
        "force_viscous",
        "force_total",
        "force_propulsive",
        "acceleration")
    ),
    current_distance = factor(current_distance)
  )

gg <- ggplot(fgen_data, aes(x = current_time, y = value, color = current_distance, group = current_distance)) +
  theme_minimal()+
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL) +
  scale_color_grey(start = 0.8, end = 0.2) +
  ggtitle("Force Generator output at different time at different distance, assuming zero velocity (BW=75kg)")

gg


# ------------------------
 x <- seq(0, 1, length.out = 1000)

 y1 <- fgen_force_time(
   current_time = x,
   initial_force = 0,
   max_force = 1000,
   time_to_max_force = 0.5
 )

 plot(x, y1, "l")

 y2 <- fgen_force_time(
   current_time = x,
   initial_force = 200,
   max_force = 1000,
   time_to_max_force = 0.5
 )

 lines(x, y2, col = "red")

 y3 <- fgen_force_time(
   current_time = x,
   initial_force = 200,
   max_force = 1000,
   time_to_max_force = 0.3
 )

 lines(x, y3, col = "blue")
