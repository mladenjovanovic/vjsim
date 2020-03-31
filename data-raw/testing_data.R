## code to prepare `testing_data` dataset goes here

require(tidyverse)
require(bmbstats)

set.seed(1667)

john_profile <- vjsim::vj_profile(
  external_load = c(0, 20, 40, 60, 80),

  # Simulation parameters
  mass = 100,
  push_off_distance = 0.45,
  max_force = 4000,
  max_velocity = 4

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

jack_profile <- vjsim::vj_profile(
  external_load = c(0, 20, 40, 60),

  # Simulation parameters
  mass = 85,
  push_off_distance = 0.35,
  max_force = 2500,
  max_velocity = 6

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

peter_profile <- vjsim::vj_profile(
  external_load = c(0, 20, 40, 60, 100),

  # Simulation parameters
  mass = 95,
  push_off_distance = 0.5,
  max_force = 4000,
  max_velocity = 6

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)


jane_profile <- vjsim::vj_profile(
  external_load = c(0, 10, 20, 30, 40),

  # Simulation parameters
  mass = 55,
  push_off_distance = 0.3,
  max_force = 2000,
  max_velocity = 4

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)


chris_profile <- vjsim::vj_profile(
  external_load = c(0, 20, 40, 60, 80),

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 5000,
  max_velocity = 3

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

# Merge profiles
testing_data <- rbind(
  data.frame(athlete = "John", john_profile),
  data.frame(athlete = "Jack", jack_profile),
  data.frame(athlete = "Peter", peter_profile),
  data.frame(athlete = "Jane", jane_profile),
  data.frame(athlete = "Chris", chris_profile)
) %>%
  select(athlete, bodyweight, push_off_distance, external_load, height) %>%
  mutate(aerial_time = get_aerial_time(height)) %>%
  select(-height) %>%
  # Add measurement noise to aerial_time
  mutate(aerial_time = aerial_time + rnorm(n = n(), 0, 0.01))

# Remove push_off_distance data from Chris
testing_data[testing_data$athlete == "Chris", ]$push_off_distance <- NA

usethis::use_data(testing_data, overwrite = TRUE)
