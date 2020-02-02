vj_probe_data <- probe_vj(
  mass = 75,
  max_force = 3000,
  max_velocity = 3,
  push_off_distance = 0.5,
  time_to_max_activation = 0.3,
  time_step = 0.001,
  peak_location = -0.1)

get_sensitivity <- function(probing_data, variable, invert = FALSE) {
   data.frame(
     change_ratio = probing_data$change_ratio,
     probing = probing_data$probing,
     variable = probing_data[[variable]]
     )
}

x <- get_sensitivity(vj_probe_data, "height")





probe_vj(
  mass = 75,
  push_off_distance = 0.4,
  max_force = 4000,
  max_velocity = 3,
  time_to_max_activation = 0.25,
  #change_ratio = vj_probing_change,
  #aggregate = vj_probing_aggregate,

  # Extra params
  weight = 75 * 9.81,
  gravity_const = 9.81,
  time_step = 0.01,
  decline_rate = 1.05,
  peak_location = - 0.05,
)
