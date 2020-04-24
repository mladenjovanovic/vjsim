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

# Install bmbstats if you haven't already by running the following commands
# devtools::install_github("mladenjovanovic/bmbstats")

# Install tidyverse and cowplot packages
# install.packages(c("tidyverse", "cowplot", "DT", "ggdendro", "ClustOfVar", "vip", "pdp"), dependencies = TRUE)

library(vjsim)
library(tidyverse)
library(cowplot)
library(DT)
library(ggdendro)
library(ClustOfVar)
library(vip)
library(pdp)
library(bmbstats)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("introduction-vjsim")
#  vignette("simulation-vjsim")
#  vignette("profiling-vjsim")
#  vignette("optimization-vjsim")

## -----------------------------------------------------------------------------
data("vjsim_data")

datatable(vjsim_data[sample(nrow(vjsim_data), 100), ], rownames = FALSE) %>%
  formatRound(columns = 1:ncol(vjsim_data), digits = 2)

## -----------------------------------------------------------------------------
colnames(vjsim_data)

## -----------------------------------------------------------------------------
plot_relationship <- function(x_var, y_var, data = vjsim_data, identity = FALSE) {
  df <- data.frame(
    x_var = data[[x_var]],
    y_var = data[[y_var]]
  )

  gg <- ggplot(df, aes(x = x_var, y = y_var)) +
    theme_cowplot(8) +
    geom_point(alpha = 0.1) +
    stat_smooth(color = "blue", method = "lm", formula = y ~ x) +
    labs(x = x_var, y = y_var)

  if (identity == TRUE) {
    gg <- gg +
      geom_abline(slope = 1, linetype = "dashed")
  }

  # Linear model
  R_squared <- cor(df$x_var, df$y_var)^2

  return(list(graph = gg, R_squared = R_squared))
}

## -----------------------------------------------------------------------------
mv_tov <- plot_relationship(
  "bodyweight_jump.mean_velocity",
  "bodyweight_jump.take_off_velocity"
)

mv_tov$graph + geom_abline(slope = 2, linetype = "dashed")
mv_tov$R_squared

## -----------------------------------------------------------------------------
plot_relationship(
  "bodyweight_jump.mean_GRF_over_distance",
  "bodyweight_jump.mean_GRF_over_time",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "bodyweight_jump.height",
  "samozino_theoretical_profile.height",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "bodyweight_jump.height",
  "samozino_practical_profile.height",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_force",
  "profile_mean_FV.F0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_force",
  "profile_peak_FV.F0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_force",
  "samozino_practical_profile.F0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
regression_model <- function(data, target, predictors, interactions = FALSE, var_imp = "firm") {
  model_df <- select_(data, .dots = c(
    target,
    predictors
  ))

  interactions_string <- "~."
  if (interactions) interactions_string <- "~.*."

  model <- lm(as.formula(paste(target, interactions_string)), data = model_df)

  # Plot
  SESOI_upper <- sd(model_df[[target]]) * 0.2
  SESOI_lower <- -sd(model_df[[target]]) * 0.2

  predicted_target_val <- predict(model)
  comment(predicted_target_val) <- "Predicted"

  observed_target_val <- model_df[[target]]
  comment(observed_target_val) <- "Observed"


  gg <- bmbstats::plot_bland_altman(
    group_a = observed_target_val,
    group_b = predicted_target_val,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    plot_average = FALSE
  )

  # Variable importance
  pfun <- function(object, newdata) predict(object, newdata = newdata)

  variable_importance <- vip(
    model,
    train = model_df,
    method = var_imp,
    target = target,
    nsim = 10,
    metric = "rmse",
    pred_wrapper = pfun,
    all_permutations = TRUE
  ) +
    theme_cowplot(8)

  # Return object
  return(list(
    train = model_df,
    model = model,
    summary = summary(model),
    graph = gg,
    var_imp = variable_importance
  ))
}

# --------------------------------------------
# PDP_ICE plot
plot_pdp <- function(object, predictor = 2) {
  # PDP + ICE
  partial(
    object$model,
    train = object$train,
    pred.var = predictor,
    plot = TRUE,
    rug = FALSE,
    ice = TRUE,
    plot.engine = "ggplot2",
    alpha = 0.01,
  ) +
    theme_cowplot(8) +
    ylab(paste("Predicted", colnames(object$train)[1]))
}

## -----------------------------------------------------------------------------
max_force_model <- regression_model(
  data = vjsim_data,
  target = "force_generator.max_force",
  predictors = c(
    "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "samozino_practical_profile.F0"
  ),
  interactions = TRUE, var_imp = "firm"
)

max_force_model$summary
max_force_model$graph
max_force_model$var_imp
plot_pdp(max_force_model, "samozino_practical_profile.F0")
plot_pdp(max_force_model, "force_generator.bodyweight")
plot_pdp(max_force_model, "force_generator.push_off_distance")

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_velocity",
  "profile_mean_FV.V0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_velocity",
  "profile_peak_FV.V0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_velocity",
  "samozino_practical_profile.V0",
  identity = TRUE
)

## -----------------------------------------------------------------------------
max_velocity_model <- regression_model(
  data = vjsim_data,
  target = "force_generator.max_velocity",
  predictors = c(
    "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "samozino_practical_profile.V0"
  ),
  interactions = TRUE, var_imp = "firm"
)

max_velocity_model$summary
max_velocity_model$graph
max_velocity_model$var_imp
plot_pdp(max_velocity_model, "samozino_practical_profile.V0")
plot_pdp(max_velocity_model, "force_generator.bodyweight")
plot_pdp(max_velocity_model, "force_generator.push_off_distance")

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "profile_mean_FV.Pmax",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "profile_mean_power.Pmax",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "profile_peak_FV.Pmax",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "profile_peak_power.Pmax",
  identity = TRUE
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "samozino_practical_profile.Pmax",
  identity = TRUE
)

## -----------------------------------------------------------------------------
max_power_model <- regression_model(
  data = vjsim_data,
  target = "force_generator.Pmax",
  predictors = c(
    "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "samozino_practical_profile.Pmax"
  ),
  interactions = TRUE, var_imp = "firm"
)

max_power_model$summary
max_power_model$graph
max_power_model$var_imp
plot_pdp(max_power_model, "samozino_practical_profile.Pmax")
plot_pdp(max_power_model, "force_generator.bodyweight")
plot_pdp(max_power_model, "force_generator.push_off_distance")

## -----------------------------------------------------------------------------
samozino_theoretical_pred <- regression_model(
  data = vjsim_data,
  target = "bodyweight_jump.height",
  predictors = c(
    "samozino_theoretical_profile.height"
  ),
  interactions = TRUE, var_imp = "firm"
)

samozino_theoretical_pred$summary
samozino_theoretical_pred$graph

## -----------------------------------------------------------------------------
samozino_practical_pred <- regression_model(
  data = vjsim_data,
  target = "bodyweight_jump.height",
  predictors = c(
    "samozino_practical_profile.height"
  ),
  interactions = TRUE, var_imp = "firm"
)

samozino_practical_pred$summary
samozino_practical_pred$graph

## -----------------------------------------------------------------------------
simple_pred <- regression_model(
  data = vjsim_data,
  target = "bodyweight_jump.height",
  predictors = c(
    "simple_profile.height"
  ),
  interactions = TRUE, var_imp = "firm"
)

simple_pred$summary
simple_pred$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_force",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.F0_rel",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.max_velocity",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "force_generator.Pmax_rel",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
fgen_height_model <- regression_model(
  data = vjsim_data,
  target = "bodyweight_jump.height",
  predictors = c(
    "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "force_generator.max_force",
    "force_generator.max_velocity",
    "force_generator.time_to_max_activation",
    "force_generator.decline_rate",
    "force_generator.peak_location"
  ),
  interactions = TRUE, var_imp = "firm"
)

fgen_height_model$summary
fgen_height_model$graph
fgen_height_model$var_imp
plot_pdp(fgen_height_model, "force_generator.max_velocity")
plot_pdp(fgen_height_model, "force_generator.max_force")
plot_pdp(fgen_height_model, "force_generator.bodyweight")

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.F0",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.F0_rel",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.V0",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.Pmax",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.Pmax_rel",
  "bodyweight_jump.height"
)

## -----------------------------------------------------------------------------
samozino_height_model <- regression_model(
  data = vjsim_data,
  target = "bodyweight_jump.height",
  predictors = c(
    "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "samozino_practical_profile.F0",
    "samozino_practical_profile.V0"
  ),
  interactions = TRUE, var_imp = "firm"
)

samozino_height_model$summary
samozino_height_model$graph
samozino_height_model$var_imp
plot_pdp(samozino_height_model, "samozino_practical_profile.F0")
plot_pdp(samozino_height_model, "samozino_practical_profile.V0")
plot_pdp(samozino_height_model, "force_generator.bodyweight")

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.Sfv_perc",
  "probe_bodyweight_jump.velocity_to_force_height_ratio"
)

probing_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.velocity_to_force_height_ratio",
  predictors = c(
    "samozino_practical_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.Sfv_perc",
  "probe_bodyweight_jump.velocity_to_activation_height_ratio"
)

probing_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.velocity_to_activation_height_ratio",
  predictors = c(
    "samozino_practical_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "samozino_practical_profile.Sfv_perc",
  "probe_bodyweight_jump.force_to_activation_height_ratio"
)

probing_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.force_to_activation_height_ratio",
  predictors = c(
    "samozino_practical_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "simple_profile.Sfv_perc",
  "probe_bodyweight_jump.velocity_to_force_height_ratio"
)

probing_simple_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.velocity_to_force_height_ratio",
  predictors = c(
    "simple_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_simple_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "simple_profile.Sfv_perc",
  "probe_bodyweight_jump.velocity_to_activation_height_ratio"
)

probing_simple_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.velocity_to_activation_height_ratio",
  predictors = c(
    "simple_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_simple_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "simple_profile.Sfv_perc",
  "probe_bodyweight_jump.force_to_activation_height_ratio"
)

probing_simple_Sfv_perc <- regression_model(
  data = vjsim_data,
  target = "probe_bodyweight_jump.force_to_activation_height_ratio",
  predictors = c(
    "simple_profile.Sfv_perc"
  ),
  interactions = TRUE, var_imp = "firm"
)


probing_simple_Sfv_perc$graph

## -----------------------------------------------------------------------------
plot_relationship(
  "LPF_profile.slope",
  "probe_bodyweight_jump.force_height_ratio"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "LPF_profile.slope",
  "probe_bodyweight_jump.velocity_height_ratio"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "LPF_profile.slope",
  "probe_bodyweight_jump.activation_height_ratio"
)

## -----------------------------------------------------------------------------
probing_samozino <- regression_model(
  data = vjsim_data,
  target = "samozino_practical_profile.Sfv_perc",
  predictors = c(
    "probe_bodyweight_jump.velocity_to_force_height_ratio",
    "probe_bodyweight_jump.velocity_to_activation_height_ratio",
    "probe_bodyweight_jump.force_to_activation_height_ratio"
  ),
  interactions = FALSE, var_imp = "firm"
)

probing_samozino$summary
probing_samozino$graph
probing_samozino$var_imp
plot_pdp(probing_samozino, "probe_bodyweight_jump.velocity_to_force_height_ratio")

## -----------------------------------------------------------------------------
probing_LPF_slope <- regression_model(
  data = vjsim_data,
  target = "LPF_profile.slope",
  predictors = c(
    "probe_bodyweight_jump.velocity_height_ratio",
    "probe_bodyweight_jump.force_height_ratio",
    "probe_bodyweight_jump.activation_height_ratio"
  ),
  interactions = FALSE, var_imp = "firm"
)

probing_LPF_slope$summary
probing_LPF_slope$graph
probing_LPF_slope$var_imp

## -----------------------------------------------------------------------------
probing_LPF_slope <- regression_model(
  data = vjsim_data,
  target = "LPF_profile.slope",
  predictors = c(
    "probe_bodyweight_jump.velocity_to_force_height_ratio",
    "probe_bodyweight_jump.velocity_to_activation_height_ratio",
    "probe_bodyweight_jump.force_to_activation_height_ratio"
  ),
  interactions = FALSE, var_imp = "firm"
)

probing_LPF_slope$summary
probing_LPF_slope$graph
probing_LPF_slope$var_imp

## -----------------------------------------------------------------------------
plot_relationship(
  "bosco.index",
  "probe_bodyweight_jump.force_height_ratio"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "bosco.index",
  "probe_bodyweight_jump.velocity_height_ratio"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "bosco.index",
  "probe_bodyweight_jump.activation_height_ratio"
)

## -----------------------------------------------------------------------------
plot_relationship(
  "bosco.index",
  "LPF_profile.slope"
)

## -----------------------------------------------------------------------------
var_clust <- function(data, predictors) {
  # Feature clustering
  data <- select_(data, .dots = predictors)
  cluster_model <- hclustvar(data)

  var_clust <- ggdendrogram(as.dendrogram(cluster_model), rotate = TRUE)

  return(list(
    model = cluster_model,
    graph = var_clust
  ))
}

## -----------------------------------------------------------------------------
jump_metrics_cluster <- var_clust(
  vjsim_data,
  c(
    "force_generator.bodyweight",
    "bodyweight_jump.take_off_time",
    "bodyweight_jump.take_off_velocity",
    "bodyweight_jump.height",
    "bodyweight_jump.mean_GRF_over_distance",
    "bodyweight_jump.mean_velocity",
    "bodyweight_jump.work_done",
    "bodyweight_jump.impulse",
    "bodyweight_jump.mean_power",
    "bodyweight_jump.mean_RFD",
    "bodyweight_jump.mean_RPD",
    "bodyweight_jump.peak_GRF",
    "bodyweight_jump.peak_velocity",
    "bodyweight_jump.peak_power",
    "bodyweight_jump.peak_RFD",
    "bodyweight_jump.GRF_at_100ms",
    "bodyweight_jump.GRF_at_200ms",
    "bodyweight_jump.peak_RPD"
  )
)

jump_metrics_cluster$graph

## -----------------------------------------------------------------------------
part <- cutreevar(jump_metrics_cluster$model, 3)
summary(part)
plot(part)

## -----------------------------------------------------------------------------
profiles_cluster <- var_clust(
  vjsim_data,
  c(
    # Force Generator characteristics
    # "force_generator.bodyweight",
    "force_generator.push_off_distance",
    "force_generator.max_force",
    "force_generator.max_velocity",
    "force_generator.decline_rate",
    "force_generator.peak_location",
    "force_generator.time_to_max_activation",
    "force_generator.Pmax",
    "force_generator.Sfv",

    # Profiles
    "profile_mean_FV.F0",
    "profile_mean_FV.V0",
    "profile_mean_FV.Pmax",
    "profile_mean_FV.Pmax",
    "profile_mean_FV.Sfv",

    "profile_mean_power.Pmax",

    "profile_peak_FV.F0",
    "profile_peak_FV.V0",
    "profile_peak_FV.Pmax",
    "profile_peak_FV.Sfv",

    "profile_load_take_off_velocity.V0",
    "profile_load_take_off_velocity.L0",
    "profile_load_take_off_velocity.Imax",
    "profile_load_take_off_velocity.Slv",

    "profile_load_impulse.Imax",

    "LPF_profile.slope",
    "samozino_practical_profile.F0",
    "samozino_practical_profile.V0",
    "samozino_practical_profile.Pmax",
    "samozino_practical_profile.Sfv",

    "simple_profile.L0",
    "simple_profile.TOV0",
    "simple_profile.Sfv"
  )
)

profiles_cluster$graph

## -----------------------------------------------------------------------------
part <- cutreevar(profiles_cluster$model, 4)
summary(part)
plot(part)

## -----------------------------------------------------------------------------
optimization_cluster <- var_clust(
  vjsim_data,
  c(
    # Probing characteristics
    "probe_bodyweight_jump.force_height_ratio",
    "probe_bodyweight_jump.velocity_height_ratio",
    "probe_bodyweight_jump.activation_height_ratio",
    "probe_bodyweight_jump.velocity_to_force_height_ratio",
    "probe_bodyweight_jump.velocity_to_activation_height_ratio",
    "probe_bodyweight_jump.force_to_activation_height_ratio",

    # Bosco index
    "bosco.index",

    # Profiles
    "LPF_profile.slope",
    "samozino_practical_profile.Sfv_perc",
    "samozino_practical_profile.probe_IMB",

    "simple_profile.Sfv_perc",
    "simple_profile.probe_IMB"
  )
)

optimization_cluster$graph

## ----eval=FALSE---------------------------------------------------------------
#  vjsim::run_simulator()

## ----eval=FALSE---------------------------------------------------------------
#  vignette("modeling-vjsim")

