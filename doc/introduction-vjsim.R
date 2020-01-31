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

## ---- echo=FALSE, force-generator, fig.cap="Force Generator characteristics"----
knitr::include_graphics(path = "force-generator-components.png")

## ----setup--------------------------------------------------------------------
# Install vjsim if you haven't already by running the following commands
# install.packages("devtools")
# devtools::install_github("mladenjovanovic/vjsim")

# Install tidyverse and cowplot packages
# install.packages(c("tidyverse", "cowplot"), dependencies = TRUE)

library(vjsim)
library(tidyverse)
library(cowplot)

## ----force-length-characteristic, fig.cap="Force-Length characteristic"-------
parameters <- expand.grid(
  current_distance = seq(0, 0.55, length.out = 1000),
  push_off_distance = c(0.3, 0.4, 0.5),
  decline_rate = c(0.8, 1.5, 2),
  peak_location = c(-0.05, -0.1, -0.15)
)

force_length <- parameters %>%
  mutate(
    force_perc = vjsim::fgen_get_force_percentage(
      current_distance =  current_distance,
      push_off_distance = push_off_distance,
      decline_rate = decline_rate,
      peak_location = peak_location
    ),
    decline_rate_label = factor(paste("Decline rate = ", decline_rate)),
    peak_location_label = factor(paste("Peak location = ", peak_location)),
    distance_to_take_off = current_distance - push_off_distance
  )

ggplot(
  force_length,
  aes(
    x = current_distance,
    y = force_perc,
    group = factor(push_off_distance),
    color = factor(push_off_distance)
  )
) +
  theme_cowplot(8) +
  geom_line() +
  facet_grid(decline_rate_label~peak_location_label) +
  xlab("Distance (m)") +
  ylab("Force percentage") +
  labs(color = "Push-off distance")

## ----force-length-characteristic-take-off, fig.cap="Force-Length characteristic using take-off point as frame of reference"----
ggplot(
  force_length,
  aes(
    x = distance_to_take_off,
    y = force_perc,
    group = factor(push_off_distance),
    color = factor(push_off_distance)
  )
) +
  theme_cowplot(8) +
  geom_line(alpha = 0.6) +
  facet_grid(decline_rate_label~peak_location_label) +
  xlab("Distance to take-off (m)") +
  ylab("Force percentage") +
  labs(color = "Push-off distance")

## ----force-time-characteristic, fig.cap="Force-Time characteristic"-----------
parameters <- expand.grid(
  current_time = seq(0, 0.6, length.out = 1000),
  initial_activation = c(0, 0.2, 0.5),
  time_to_max_activation = c(0.3, 0.5)
)

force_time <- parameters %>%
  mutate(
    activation = vjsim::fgen_get_activation(
      current_time = current_time,
      initial_activation = initial_activation,
      time_to_max_activation = time_to_max_activation
    ),
    initial_activation_label = factor(initial_activation),
    time_to_max_activation_label = factor(paste("Time to max activation =", time_to_max_activation)),
  )

ggplot(
  force_time,
  aes(
    x = current_time,
    y = activation,
    group = initial_activation_label,
    color = initial_activation_label
  )
) +
  theme_cowplot(12) +
  geom_line() +
  facet_wrap(~time_to_max_activation_label) +
  xlab("Time (s)") +
  ylab("Activation") +
  labs(color = "Initial activation")

## ----initial-activation, fig.cap="Calculating initial activation"-------------
parameters <- expand.grid(
  current_time = seq(0, 0.5, length.out = 1000),
  max_force = 3000,
  weight = c(75 * 9.81, 85 * 9.81),
  force_percentage = c(0.6, 0.8),
  time_to_max_activation = c(0.3, 0.5)
)

force_time <- parameters %>%
  mutate(
    potential_force = max_force * force_percentage,
    initial_activation = weight / potential_force,

    activation = vjsim::fgen_get_activation(
      current_time = current_time,
      initial_activation = initial_activation,
      time_to_max_activation = time_to_max_activation
    ),

    generated_force = activation * potential_force,

    weight_label = factor(weight),
    time_to_max_activation_label = factor(paste("Time to max activation =", time_to_max_activation)),
    force_percentage_label = factor(force_percentage)
  )

ggplot(
  force_time,
  aes(
    x = current_time,
    y = activation,
    color = weight_label,
    linetype = force_percentage_label
  )
) +
  theme_cowplot(12) +
  geom_line() +
  facet_wrap(~time_to_max_activation_label) +
  xlab("Time (s)") +
  ylab("Activation") +
  labs(color = "Weight (N)", linetype = "Force Percentage")

ggplot(
  force_time,
  aes(
    x = current_time,
    y = generated_force,
    color = weight_label,
    linetype = force_percentage_label
  )
) +
  theme_cowplot(12) +
  geom_line() +
  facet_wrap(~time_to_max_activation_label) +
  xlab("Time (s)") +
  ylab("Generated Force (N)") +
  labs(color = "Weight (N)", linetype = "Force Percentage")

## ----force-velocity-characteristic, fig.cap="Force-Velocity characteristic"----
parameters <- expand.grid(
  current_velocity = seq(0, 4.5, length.out = 1000),
  max_force = c(2000, 2500, 3000),
  max_velocity = c(2, 3, 4)
)

force_velocity <- parameters %>%
  mutate(
    force = vjsim::fgen_get_viscous_force(
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
  theme_cowplot(12) +
  geom_hline(aes(yintercept = max_force), linetype = "dashed") +
  geom_line() +
  facet_wrap(~max_force_label) +
  xlab("Velocity (m/s)") +
  ylab("Viscous Force (N)") +
  labs(color = "Max Velocity")

## ----fig.cap="Relationship between external force and velocity that can be reached"----
parameters <- expand.grid(
  external_force = seq(0, 3000, length.out = 1000),
  max_force = c(2000, 3000),
  max_velocity = c(3, 4)
)

velocity_reached <- parameters %>%
  filter(
    external_force < max_force
  ) %>%
  mutate(
    velocity_reached = vjsim::fgen_get_velocity(
      external_force = external_force,
      max_force = max_force,
      max_velocity = max_velocity
    ),
    max_velocity = factor(max_velocity),
    max_force_label = factor(max_force),
    id = paste(max_force, max_velocity)
  ) %>%
  filter(id %in% c("2000 4", "3000 3"))

ggplot(
  velocity_reached,
  aes(
    x = external_force,
    y = velocity_reached,
    color = max_velocity,
    linetype = max_force_label
  )
) +
  theme_cowplot(12) +
  geom_line() +
  ylab("Velocity reached (m/s)") +
  xlab("External Force (N)") +
  labs(color = "Max Velocity", linetype = "Max Force")

## ----fig.cap="Relationship between external force and power"------------------
velocity_reached <- velocity_reached %>%
  mutate(
    power = external_force * velocity_reached
  )

ggplot(
  velocity_reached,
  aes(
    x = external_force,
    y = power,
    color = max_velocity,
    linetype = max_force_label
  )
) +
  theme_cowplot(12) +
  geom_line() +
  ylab("Power (W)") +
  xlab("External Force (N)") +
  labs(color = "Max Velocity", linetype = "Max Force")

## ---- echo=FALSE, schematic-diagram, fig.cap="Schematic diagrom of the components, parameters, and resulting forces in vjsim"----
knitr::include_graphics(path = "schematic-diagram.png")

## -----------------------------------------------------------------------------
state <- expand.grid(
  current_time = seq(0, 0.25, length.out = 1000),
  current_distance = c(0.1, 0.15, 0.2, 0.25, 0.3),
  current_velocity = 0
)

fgen_data <- vjsim::fgen_get_output(
  current_time = state$current_time,
  current_distance = state$current_distance,
  current_velocity = state$current_velocity,

  mass = 75,
  push_off_distance = 0.4,

  max_force = 3000,
  max_velocity = 4,

  decline_rate = 1.5,
  peak_location = -0.1,

  time_to_max_activation = 0.3
)

as_tibble(as.data.frame(fgen_data))

## ----eval=FALSE---------------------------------------------------------------
#  vignette("simulation-vjsim")

