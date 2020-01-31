require(tidyverse)

vj_probe_data <- probe_vj(
  mass = 75,
  max_force = 3000,
  max_velocity = 3,
  time_to_max_activation = 0.3,
  time_step = 0.001
)

# Invert for mass and time_to_max_activation
vj_probe_data$change_ratio <- ifelse(
  vj_probe_data$probing == "time_to_max_activation",
  1 / vj_probe_data$change_ratio,
  vj_probe_data$change_ratio
)

vj_probe_data$change_ratio <- ifelse(
  vj_probe_data$probing == "mass",
  1 / vj_probe_data$change_ratio,
  vj_probe_data$change_ratio
)


plot_data <- gather(vj_probe_data, key = "variable", value = "value", -(1:9)) %>%
  filter(variable %in% c(
    "height",
    "current_time",
    "mean_velocity",
    "peak_velocity",
    "take_off_velocity",
    "mean_GRF_over_distance",
    "mean_GRF_over_time",
    "peak_GRF",
    "peak_power",
    "mean_power",
    "peak_RFD",
    "peak_RPD"
  ))

plot_data$reverse <- plot_data$probing %in% c("mass", "time_to_max_activation")

ggplot(plot_data, aes(x = change_ratio, y = value, color = probing, linetype = reverse)) +
  theme_minimal() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  xlab("Normalized parameter change") +
  ylab(NULL) +
  scale_color_manual(values = c(
    "mass" = "#4D4D4D",
    "max_force" = "#5DA5DA",
    "max_velocity" =  "#FAA43A",
    "push_off_distance" = "#60BD68",
    "time_to_max_activation" = "#B276B2"))
