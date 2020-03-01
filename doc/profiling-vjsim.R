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

## ----setup--------------------------------------------------------------------
# Install vjsim if you haven't already by running the following commands
# install.packages("devtools")
# devtools::install_github("mladenjovanovic/vjsim")

# Install tidyverse and cowplot packages
# install.packages(c("tidyverse", "cowplot", "DT), dependencies = TRUE)

library(vjsim)
library(tidyverse)
library(cowplot)
library(DT)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("introduction-vjsim")
#  vignette("simulation-vjsim")

## -----------------------------------------------------------------------------
# External load in kilograms
external_load <- c(0, 20, 40, 60, 80, 100)

## -----------------------------------------------------------------------------
profile_data <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  max_velocity = 4

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

## -----------------------------------------------------------------------------
datatable(profile_data, rownames = FALSE) %>%
  formatRound(columns = 1:ncol(profile_data), digits = 2)

## -----------------------------------------------------------------------------
plot_profile <- function(profile_data, x_var, y_var) {
  df <- data.frame(
    x_var = profile_data[[x_var]],
    y_var = profile_data[[y_var]]
  )

  gg <- ggplot(df, aes(x = x_var, y = y_var)) +
    theme_cowplot(8) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(x = x_var, y = y_var)

  return(gg)
}

## -----------------------------------------------------------------------------
plot_profile(profile_data, "external_load", "height")

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "height")

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "take_off_velocity")

## -----------------------------------------------------------------------------
lm_model <- lm(take_off_velocity~mass, profile_data)

model_data <- tibble(
  mass = seq(-50, 300),
  predicted_tov = predict(lm_model, newdata = data.frame(mass = mass))
)

plot_profile(profile_data, "mass", "take_off_velocity") +
  geom_line(data = model_data, aes(y = predicted_tov, x = mass), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "take_off_velocity"
)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "external_load",
  velocity = "take_off_velocity"
)

## -----------------------------------------------------------------------------
lm_model <- lm(height~poly(mass, 2), profile_data)

model_data <- tibble(
  mass = seq(-10, 250),
  predicted_height = predict(lm_model, newdata = data.frame(mass = mass))
)

plot_profile(profile_data, "mass", "height") +
  geom_line(data = model_data, aes(y = predicted_height, x = mass), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
lm_model <- lm(height~poly(mass, 3), profile_data)

model_data <- tibble(
  mass = seq(-10, 250),
  predicted_height = predict(lm_model, newdata = data.frame(mass = mass))
)

plot_profile(profile_data, "mass", "height") +
  geom_line(data = model_data, aes(y = predicted_height, x = mass), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "height", 
  poly_deg = 3
)

## -----------------------------------------------------------------------------
lm_model <- lm(height~poly(mass, 1), profile_data)

model_data <- tibble(
  mass = seq(-10, 250),
  predicted_height = predict(lm_model, newdata = data.frame(mass = mass))
)

plot_profile(profile_data, "mass", "height") +
  geom_line(data = model_data, aes(y = predicted_height, x = mass), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "height"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity")

## -----------------------------------------------------------------------------
lm_model <- lm(mean_velocity~poly(mean_GRF_over_distance, 1), profile_data)

model_data <- tibble(
  mean_GRF_over_distance = seq(-50, 3500),
  predicted_mv = predict(lm_model, newdata = data.frame(mean_GRF_over_distance = mean_GRF_over_distance))
)

plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity") +
  geom_line(data = model_data, aes(y = predicted_mv, x = mean_GRF_over_distance), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity"
)

## -----------------------------------------------------------------------------
lm_model <- lm(mean_velocity~poly(mean_GRF_over_distance, 2), profile_data)

model_data <- tibble(
  mean_GRF_over_distance = seq(-50, 3500),
  predicted_mv = predict(lm_model, newdata = data.frame(mean_GRF_over_distance = mean_GRF_over_distance))
)

plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity") +
  geom_line(data = model_data, aes(y = predicted_mv, x = mean_GRF_over_distance), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity",
  poly_deg = 2
)

## -----------------------------------------------------------------------------
lm_model <- lm(mean_velocity~poly(mean_GRF_over_distance, 3), profile_data)

model_data <- tibble(
  mean_GRF_over_distance = seq(-50, 3500),
  predicted_mv = predict(lm_model, newdata = data.frame(mean_GRF_over_distance = mean_GRF_over_distance))
)

plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity") +
  geom_line(data = model_data, aes(y = predicted_mv, x = mean_GRF_over_distance), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity",
  poly_deg = 3
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mean_GRF_over_distance", "mean_power")

## -----------------------------------------------------------------------------
plot_profile(profile_data, "external_load", "mean_power")

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "mean_power")

## -----------------------------------------------------------------------------
lm_model <- lm(mean_power~poly(mean_GRF_over_distance, 2), profile_data)

model_data <- tibble(
  mean_GRF_over_distance = seq(-50, 3500),
  predicted_mp = predict(lm_model, newdata = data.frame(mean_GRF_over_distance = mean_GRF_over_distance))
)

plot_profile(profile_data, "mean_GRF_over_distance", "mean_power") +
  geom_line(data = model_data, aes(y = predicted_mp, x = mean_GRF_over_distance), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)

## -----------------------------------------------------------------------------
vjsim::get_power_profile(
  profile_data = profile_data,
  x_var = "mean_GRF_over_distance",
  power = "mean_power"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "peak_GRF", "peak_velocity")

## -----------------------------------------------------------------------------
vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "peak_GRF",
  velocity = "peak_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "peak_GRF", "peak_power")

## -----------------------------------------------------------------------------
vjsim::get_power_profile(
  profile_data = profile_data,
  x_var = "peak_GRF",
  power = "peak_power"
)

## -----------------------------------------------------------------------------
all_profiles <- vjsim::get_all_profiles(
  profile_data = profile_data
)

datatable(all_profiles$data_frame, rownames = FALSE) %>%
  formatRound(columns = 1:ncol(all_profiles$data_frame), digits = 2)

## -----------------------------------------------------------------------------
probe_data <- probe_profile(
  mass = 75,
  max_force = 3000,
  max_velocity = 4,
  time_to_max_activation = 0.3,
  time_step = 0.001,
  external_load = c(0, 20, 40, 60, 80, 100),
  change_ratio = seq(0.9, 1.1, length.out = 3),

  # Profile variables
  profile_func = function(...) {
    list(list = get_FV_profile(
      ...,
      force = "mass",
      velocity = "take_off_velocity"
    ))
  },
  aggregate = "ratio"
)

datatable(probe_data, rownames = FALSE) %>%
  formatRound(columns = 1:ncol(probe_data), digits = 2)

## -----------------------------------------------------------------------------
# Need this package to label the lines
# install.packages("directlabels")
require(directlabels) 

plot_probe <- function(probing_data) {
  # Convert to long
  probe_data <- gather(probe_data, key = "variable", value = "value", -(1:8))
  
  gg <- ggplot(probe_data,
    aes(x = change_ratio, y = value, color = probing)
    ) +
   theme_cowplot(6) +
   geom_line() +
   facet_wrap(~variable, scales = "free_y") +
   xlab("Normalized parameter change") +
   ylab("Normalized profile change") +
   scale_color_manual(values = c(
     "mass" = "#4D4D4D",
     "max_force" = "#5DA5DA",
     "max_velocity" =  "#FAA43A",
     "push_off_distance" = "#60BD68",
     "time_to_max_activation" = "#B276B2")) +
    xlim(c(0.9, 1.2)) 
    
  fgen_facets <- direct.label(gg, list("last.bumpup", cex = 0.4))
  
  gg <- ggplot(probe_data,
  aes(x = change_ratio, y = value, color = variable)) +
   theme_cowplot(8) +
   geom_line() +
   facet_wrap(~probing, scales = "free_y") +
   xlab("Normalized parameter change") +
   ylab("Normalized profile change") +
   xlim(c(0.9, 1.2))
  
  profile_facets <- direct.label(gg, list("last.bumpup", cex = 0.4))
  
  
  return(list(
    fgen_facets = fgen_facets,
    profile_facets = profile_facets)
  )
}

## -----------------------------------------------------------------------------
plot_probe(probe_data)

## -----------------------------------------------------------------------------
probe_data <- probe_profile(
  mass = 75,
  max_force = 3000,
  max_velocity = 4,
  time_to_max_activation = 0.3,
  time_step = 0.001,
  external_load = external_load,
  change_ratio = seq(0.9, 1.1, length.out = 3),

  # Profile variables
  profile_func = function(...) {
    list(list = get_FV_profile(
      ...,
      force = "mean_GRF_over_distance",
      velocity = "mean_velocity"
    ))
  },
  aggregate = "ratio"
)

plot_probe(probe_data)

## -----------------------------------------------------------------------------
profile_data_original <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  max_velocity = 4

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

profile_data_velocity <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  max_velocity = 4 * 1.1,

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

profile_data_force <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000 * 1.1, 
  max_velocity = 4

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

x_var <- "mean_GRF_over_distance"
y_var <- "mean_velocity"

profile_probe_data <- rbind(
  data.frame(
    profile = "original",
    force = profile_data_original[[x_var]],
    velocity = profile_data_original[[y_var]]
    ),
  
    data.frame(
    profile = "velocity",
    force = profile_data_velocity[[x_var]],
    velocity = profile_data_velocity[[y_var]]
    ),
  
    data.frame(
    profile = "force",
    force = profile_data_force[[x_var]],
    velocity = profile_data_force[[y_var]]
    )
)

gg <- ggplot(profile_probe_data, aes(x = force, y = velocity, color = profile)) +
  theme_cowplot(8) +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8)
gg

## -----------------------------------------------------------------------------
gg <- ggplot(profile_probe_data, aes(x = force, y = velocity, color = profile)) +
  theme_cowplot(8) +
  #geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = lm, se=FALSE, alpha = 0.5, fullrange=TRUE, size = 0.5, linetype = "dashed") +
  xlim(-10, 3800) +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)
gg

## -----------------------------------------------------------------------------
x_var <- "mass"
y_var <- "take_off_velocity"

profile_probe_data <- rbind(
  data.frame(
    profile = "original",
    force = profile_data_original[[x_var]],
    velocity = profile_data_original[[y_var]]
    ),
  
    data.frame(
    profile = "velocity",
    force = profile_data_velocity[[x_var]],
    velocity = profile_data_velocity[[y_var]]
    ),
  
    data.frame(
    profile = "force",
    force = profile_data_force[[x_var]],
    velocity = profile_data_force[[y_var]]
    )
)

gg <- ggplot(profile_probe_data, aes(x = force, y = velocity, color = profile)) +
  theme_cowplot(8) +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8)
gg

## -----------------------------------------------------------------------------
gg <- ggplot(profile_probe_data, aes(x = force, y = velocity, color = profile)) +
  theme_cowplot(8) +
  #geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = lm, se=FALSE, alpha = 0.5, fullrange=TRUE, size = 0.5, linetype = "dashed") +
  xlim(-10, 300) +
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)
gg


## -----------------------------------------------------------------------------
# Install bmbstats if you haven't already by running the following commands
# install.packages("devtools")
# devtools::install_github("mladenjovanovic/bmbstats")

library(bmbstats)

## -----------------------------------------------------------------------------
boot_profile <- function(profile_data, force = "mass", velocity = "take_off_velocity", poly_deg = 1) {
  
  profile_estimators <- function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
    # Get profile
    profile <- vjsim::get_FV_profile(
         profile_data = data,
         force = force,
         velocity = velocity,
         poly_deg = poly_deg
    )
    
    # Return profile
    return(profile)
  }
  
  # Perform bootstrap
  boot_data <- bmbstats::bmbstats(
    data = profile_data,
    estimator_function = profile_estimators,
    boot_samples = 1000,
    boot_type = "perc"
    )
  
  # Add plot
  plot_data <- data.frame(F0 = boot_data$boot$t[, 1], V0 = boot_data$boot$t[, 2])
  
  n_points <- nrow(plot_data)
  
  plot_data <- data.frame(
    x = c(plot_data$F0, rep(0, n_points)),
    y = c(rep(0, n_points), plot_data$V0),
    group = c(seq(1, n_points), seq(1, n_points))
  )
  
  gg <- ggplot(plot_data, aes(x = x, y = y, group = group)) +
    theme_cowplot(8) +
    geom_line(alpha = 0.01, color = "blue") +
    labs(x = force, y = velocity)
  
  boot_data$graphs <- gg
  
  return(boot_data)
}

## -----------------------------------------------------------------------------
boot_data <- boot_profile(profile_data, force = "mass", velocity = "take_off_velocity")

## -----------------------------------------------------------------------------
boot_data$estimators

## -----------------------------------------------------------------------------
boot_data$graphs

## -----------------------------------------------------------------------------
boot_data <- boot_profile(profile_data, force = "mean_GRF_over_distance", velocity = "take_off_velocity")

boot_data$estimators

boot_data$graphs

## -----------------------------------------------------------------------------
# Profile data for Force Generator with only viscous components
profile_data <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  max_velocity = 4,

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  
  # Setting these to 0 removes these components from the Force Generator
  decline_rate = 0,
  peak_location = 0,
  time_to_max_activation = 0
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "height")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "height"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "take_off_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "take_off_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "peak_GRF", "peak_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "peak_GRF",
  velocity = "peak_velocity"
)

## -----------------------------------------------------------------------------
# Profile data for Force Generator with only viscous components
profile_data <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  
  max_velocity = Inf, # Needs to be infinite

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  
  # Setting these to 0 removes these components from the Force Generator
  decline_rate = 0,
  peak_location = 0,
  time_to_max_activation = 0
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "height")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "height"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "take_off_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "take_off_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "peak_GRF", "peak_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "peak_GRF",
  velocity = "peak_velocity"
)

## -----------------------------------------------------------------------------
# Profile data for Force Generator with only viscous components
profile_data <- vjsim::vj_profile(
  external_load = external_load,

  # Simulation parameters
  mass = 75,
  push_off_distance = 0.4,
  max_force = 3000,
  
  max_velocity = Inf, # Needs to be infinite

  # Other parameters are default in the `vjsim::fgen_get_output`
  # weight = mass * gravity_const,
  # gravity_const = 9.81,
  # decline_rate = 1.05,
  # peak_location = -push_off_distance * 0.15,
  # time_to_max_activation = 0.3
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "height")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "height"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mass", "take_off_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mass",
  velocity = "take_off_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "mean_GRF_over_distance", "mean_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "mean_GRF_over_distance",
  velocity = "mean_velocity"
)

## -----------------------------------------------------------------------------
plot_profile(profile_data, "peak_GRF", "peak_velocity")

vjsim::get_FV_profile(
  profile_data = profile_data,
  force = "peak_GRF",
  velocity = "peak_velocity"
)

## ----eval=FALSE---------------------------------------------------------------
#  vjsim::run_simulator()

## ----eval=FALSE---------------------------------------------------------------
#  vignette("optimization-vjsim")

