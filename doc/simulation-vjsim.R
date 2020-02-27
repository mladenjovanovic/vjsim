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

## -----------------------------------------------------------------------------
kinetics_data <- tibble(
  GRF = 1000, # Ground reaction force; in Newtons
  mass = 10, # kg
  gravity_const = 9.81, # For Earth
  weight = mass * gravity_const, # In Newtons
  propulsive_force = GRF - weight,
  acceleration = propulsive_force / mass,
  time = seq(0, 10, length.out = 100)
)

plot_data <- gather(kinetics_data, "variable", "value", -time) %>%
  mutate(variable = factor(variable, levels = c(
    "GRF",
    "mass",
    "gravity_const",
    "weight",
    "propulsive_force",
    "acceleration"
  )))

ggplot(plot_data, aes(x = time, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
velocity_0 <- 0 # Initial velocity
time_step <- 0.01

# Init the column in data frame
kinetics_data$velocity <- NA

# Save the initial velocity
kinetics_data$velocity[1] <- velocity_0

for (i in seq(1, nrow(kinetics_data)-1)){
    velocity_current <- kinetics_data$velocity[i]
    acceleration_current <- kinetics_data$acceleration[i]
    
    velocity_next <- velocity_current + acceleration_current * time_step
    
    # Save the next velocity
    kinetics_data$velocity[i+1] <- velocity_next
}

## -----------------------------------------------------------------------------
plot_data <- gather(kinetics_data, "variable", "value", -time) %>%
  mutate(variable = factor(variable, levels = c(
    "GRF",
    "mass",
    "gravity_const",
    "weight",
    "propulsive_force",
    "acceleration",
    "velocity"
  )))

ggplot(plot_data, aes(x = time, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
distance_0 <- 0 # Initial distance
time_step <- 0.01

# Init the column in data frame
kinetics_data$distance <- NA

# Save the initial velocity
kinetics_data$distance[1] <- distance_0

for (i in seq(1, nrow(kinetics_data)-1)){
    distance_current <- kinetics_data$distance[i]
    velocity_current <- kinetics_data$velocity[i]
    
    distance_next <- distance_current + velocity_current * time_step
    
    # Save the next distance
    kinetics_data$distance[i+1] <- distance_next
}

## -----------------------------------------------------------------------------
plot_data <- gather(kinetics_data, "variable", "value", -time) %>%
  mutate(variable = factor(variable, levels = c(
    "GRF",
    "mass",
    "gravity_const",
    "weight",
    "propulsive_force",
    "acceleration",
    "velocity",
    "distance"
  )))

ggplot(plot_data, aes(x = time, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
kinetics_data <- kinetics_data %>%
  mutate(power = GRF * velocity)

plot_data <- gather(kinetics_data, "variable", "value", -time) %>%
  mutate(variable = factor(variable, levels = c(
    "GRF",
    "mass",
    "gravity_const",
    "weight",
    "propulsive_force",
    "acceleration",
    "velocity",
    "distance",
    "power"
  )))

ggplot(plot_data, aes(x = time, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") +
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
ggplot(kinetics_data, aes(x = time, y = velocity)) +
  theme_cowplot() +
  geom_line(color = "blue") +
  geom_hline(yintercept = max(kinetics_data$velocity)/2, linetype = "dashed")

## -----------------------------------------------------------------------------
ggplot(kinetics_data, aes(x = distance, y = velocity)) +
  theme_cowplot() +
  geom_line(color = "blue")

## -----------------------------------------------------------------------------
vertical_jump <- vjsim::vj_simulate(
   mass = 85,
   weight = 85 * 9.81,
   push_off_distance = 0.4,
   gravity_const = 9.81,
   time_step = 0.001,
   
   # Parameters of the Force Generator
   max_force = 3000,
   max_velocity = 4,
   decline_rate = 1.05,
   peak_location = -0.06,
   time_to_max_activation = 0.3
)

## -----------------------------------------------------------------------------
datatable(vertical_jump$trace, rownames = FALSE) %>%
      formatRound(columns = 1:ncol(vertical_jump$trace), digits = 3)

## -----------------------------------------------------------------------------
plot_data <- gather(vertical_jump$trace, "variable", "value", -(1:15)) %>%
  mutate(variable = factor(
    variable,
    levels = c(
      "force_percentage",
      "potential_force",
      "activation",
      "generated_force",
      "viscous_force",
      "ground_reaction_force",
      "propulsive_force",
      "acceleration",
      "power",
      "RFD",
      "RPD"
    )))

ggplot(plot_data, aes(x = time, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") + 
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
ggplot(plot_data, aes(x = distance, y = value)) +
  theme_cowplot(8) +
  geom_line(color = "blue") + 
  facet_wrap(~variable, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
df <- as.data.frame(t(vertical_jump$summary))
colnames(df)[1] <- "value"
round(df, 2)

## ----eval=FALSE---------------------------------------------------------------
#  vjsim::run_simulator()

## -----------------------------------------------------------------------------
probing_data <- vjsim::probe_vj(
   mass = 85,
   change_ratio = c(0.9, 1, 1.1),
   aggregate = "raw",
   # Parameters forwarded to vj_sim
   weight = 85 * 9.81,
   push_off_distance = 0.4,
   gravity_const = 9.81,
   time_step = 0.001,
   # Parameters of the Force Generator
   max_force = 3000,
   max_velocity = 4,
   decline_rate = 1.05,
   peak_location = -0.06,
   time_to_max_activation = 0.3
)

datatable(probing_data, rownames = FALSE) %>%
      formatRound(columns = 1:ncol(probing_data), digits = 2)

## -----------------------------------------------------------------------------
# Invert for mass and time_to_max_activation
 probing_data$change_ratio <- ifelse(
   probing_data$probing == "time_to_max_activation",
   1 / probing_data$change_ratio,
   probing_data$change_ratio
 )

 probing_data$change_ratio <- ifelse(
   probing_data$probing == "mass",
   1 / probing_data$change_ratio,
   probing_data$change_ratio
 )


 plot_data <- gather(probing_data, key = "variable", value = "value", -(1:13)) %>%
   filter(variable %in% c(
     "height",
     "take_off_time",
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

gg <- ggplot(plot_data, aes(x = change_ratio, y = value, color = probing, linetype = reverse)) +
   theme_cowplot(6) +
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
gg

## -----------------------------------------------------------------------------
gg <-  ggplot(filter(plot_data, variable == "height"),
  aes(x = change_ratio, y = value, color = probing, linetype = reverse)) +
   theme_cowplot(12) +
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
gg

## -----------------------------------------------------------------------------
# Need this package to label the lines
# install.packages("directlabels")
require(directlabels) 


probing_data <- vjsim::probe_vj(
   mass = 85,
   change_ratio = c(0.9, 0.95, 1, 1.05, 1.1),
   aggregate = "ratio",
   # Parameters forwarded to vj_sim
   weight = 85 * 9.81,
   push_off_distance = 0.4,
   gravity_const = 9.81,
   time_step = 0.001,
   # Parameters of the Force Generator
   max_force = 3000,
   max_velocity = 4,
   decline_rate = 1.05,
   peak_location = -0.06,
   time_to_max_activation = 0.3
)

 plot_data <- gather(probing_data, key = "variable", value = "value", -(1:13)) %>%
   filter(
     variable %in% c(
     "height",
     "take_off_time",
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
   )
   )

gg <- ggplot(filter(plot_data, probing == "max_force"),
  aes(x = change_ratio, y = value, color = variable)) +
   theme_cowplot(12) +
   geom_line() +
   facet_wrap(~probing, scales = "free_y") +
   xlab("Normalized parameter change") +
   ylab(NULL) +
   xlim(c(0.9, 1.2))
direct.label(gg, list("last.bumpup", cex = 0.7))

## -----------------------------------------------------------------------------
gg <- ggplot(plot_data,
  aes(x = change_ratio, y = value, color = variable)) +
   theme_cowplot(8) +
   geom_line() +
   facet_wrap(~probing, scales = "free_y") +
   xlab("Normalized parameter change") +
   ylab(NULL) +
   xlim(c(0.9, 1.2))
direct.label(gg, list("last.bumpup", cex = 0.4))

## ----eval=FALSE---------------------------------------------------------------
#  vjsim::run_simulator()

## ----eval=FALSE---------------------------------------------------------------
#  vignette("profiling-vjsim")

