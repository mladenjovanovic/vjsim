## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4,
  dpi = 300,
  out.width = "90%",
  auto_pdf = TRUE,
  message = FALSE,
  warning = FALSE
)

## ---- echo=FALSE, mechanical-model, fig.cap="Mechanical model that is used to represent vertical jump"----
knitr::include_graphics(path = "mechanical-model.png")

## ---- echo=FALSE, force-generator, fig.cap="Force Generator components"-------
knitr::include_graphics(path = "force-generator-components.png")

## ----setup--------------------------------------------------------------------
# Install vjism if you haven't but running the following commands
# install.packages("devtools")
# devtools::install_github("mladenjovanovic/vjsim")

library(vjsim)
library(tidyverse)

## ----force-length-relationship, fig.cap="Force-Length relationhsip"-----------
parameters <- expand.grid(
  current_distance = seq(0, 0.7, length.out = 1000),
  start_force = c(0.6, 0.8),
  end_force = 0,
  threshold = c(0.8, 0.9),
  push_off_distance = c(0.4, 0.7)
)

force_length <- parameters %>%
  mutate(
    force_percentage = vjsim::fgen_force_length(
      current_distance = current_distance,
      start_force = start_force,
      end_force = end_force,
      threshold = threshold,
      push_off_distance = push_off_distance
    ),
    threshold = factor(threshold),
    start_force = factor(paste("Start force =", start_force)),
    push_off_distance = factor(paste("Push-off =", push_off_distance)),
  )

ggplot(
  force_length,
  aes(
    x = current_distance,
    y = force_percentage,
    group = threshold,
    color = threshold
  )
) +
  theme_minimal() +
  geom_line() +
  facet_grid(push_off_distance ~ start_force) +
  xlab("Distance (m)") +
  ylab("Force percentage")

## ----force-time-relationship, fig.cap="Force-Time relationhsip"---------------
parameters <- expand.grid(
  current_time = seq(0, 0.6, length.out = 1000),
  initial_force = c(0, 735),
  max_force = 2700,
  time_to_max_force = c(0.3, 0.5)
)

force_time <- parameters %>%
  mutate(
    force = vjsim::fgen_force_time(
      current_time = current_time,
      initial_force = initial_force,
      max_force = max_force,
      time_to_max_force = time_to_max_force
    ),
    initial_force = factor(initial_force),
    time_to_max_force = factor(paste("Time to max force =", time_to_max_force)),
  )

ggplot(
  force_time,
  aes(
    x = current_time,
    y = force,
    group = initial_force,
    color = initial_force
  )
) +
  theme_minimal() +
  geom_line() +
  facet_wrap(~time_to_max_force) +
  xlab("Time (s)") +
  ylab("Force (N)")

## ----force-velocity-relationship, fig.cap="Force-Velocity relationhsip"-------
parameters <- expand.grid(
  current_velocity = seq(0, 4.5, length.out = 1000),
  max_force = c(2000, 2500, 3000),
  max_velocity = c(2, 3, 4)
)

force_velocity <- parameters %>%
  mutate(
    force = vjsim::fgen_force_velocity(
      current_velocity = current_velocity,
      max_force = max_force,
      max_velocity = max_velocity
    ),
    max_velocity = factor(max_velocity),
    max_force_label = factor(paste("Max Force = ", max_force))
  )

ggplot(
  force_velocity,
  aes(
    x = current_velocity,
    y = force,
    group = max_velocity,
    color = max_velocity
  )
) +
  theme_minimal() +
  geom_hline(aes(yintercept = -max_force), linetype = "dashed") +
  geom_line() +
  facet_wrap(~max_force_label) +
  xlab("Velocity (m/s)") +
  ylab("Viscous Force (N)")

## -----------------------------------------------------------------------------
parameters <- expand.grid(
  external_resistance = seq(0, 3000, length.out = 1000),
  max_force = c(2000, 2500, 3000),
  max_velocity = c(2, 3, 4)
)

velocity_reached <- parameters %>%
  filter(external_resistance < max_force) %>%
  mutate(
    velocity_reached = vjsim::fgen_get_velocity(
      external_resistance = external_resistance,
      max_force = max_force,
      max_velocity = max_velocity
    ),
    max_velocity = factor(max_velocity),
    max_force_label = factor(paste("Max force =", max_force))
  )

ggplot(
  velocity_reached,
  aes(
    x = external_resistance,
    y = velocity_reached,
    group = max_velocity,
    color = max_velocity
  )
) +
  theme_minimal() +
  geom_line() +
  facet_wrap(~max_force_label) +
  ylab("Velocity reached (m/s)") +
  xlab("External Force (N)")

