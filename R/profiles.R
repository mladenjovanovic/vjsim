#' Vertical Jump Profile
#'
#' \code{vj_profile} generates vertical jump profile by loading the vertical jump with \code{external_load}
#' @param external_load Numeric vector. Default is  \code{c(-40, -20, 0, 20, 40, 60, 80)}
#' @param mass Numeric value. Represents bodyweight
#' @param ... Parameters forwarded to \code{\link{vj_simulate}}
#' @return Data frame. Returns the data frame from \code{\link{vj_simulate}} with extra two columns:
#'     \code{bodyweigh}t and \code{external_load}
#' @export
#' @examples
#' vj_profile_data <- vj_profile(mass = 75)
#'
#' plot(x = vj_profile_data$mass, y = vj_profile_data$height)
#' plot(x = vj_profile_data$mean_GRF_over_distance, y = vj_profile_data$mean_velocity)
vj_profile <- function(external_load = c(-40, -20, 0, 20, 40, 60, 80, 100),
                       mass = 75,
                       ...) {
  profile <- data.frame()
  bw <- mass

  for (i in seq_along(external_load)) {
    new_mass <- external_load[i] + mass
    vj_summary <- vj_simulate(mass = new_mass, save_trace = FALSE, ...)
    vj_summary <- vj_summary$summary

    profile <- rbind(
      profile,
      data.frame(
        bodyweight = bw,
        external_load = external_load[i],
        vj_summary
      )
    )
  }
  return(profile)
}

#' Get Force-Velocity profile
#'
#' Applied simple linear regression model using \code{force} and \code{velocity} columns from the \code{profile_data} and
#'     returns calculated \code{F0}, \code{V0}, \code{Pmax}, and \code{Sfv}
#' @param profile_data Data frame returned from \code{\link{vj_profile}} function
#' @param force Character string. Name of the column in the \code{profile_data}. Default is "mean_GRF_over_distance"
#' @param velocity Character string. Name of the column in the \code{profile_data}. Default is "mean_velocity"
#' @param poly_deg Integer. Number of polynomial degrees. Forwarded to \code{\link[stats]{poly}} function.
#'     Default is 1 (for simple linear regression)
#' @return List with calculated \code{F0}, \code{V0}, \code{Pmax}, and \code{Sfv}
#' @export
#' @examples
#' fv_profile <- vj_profile(mass = 75)
#' get_FV_profile(fv_profile)
get_FV_profile <- function(profile_data, force = "mean_GRF_over_distance", velocity = "mean_velocity", poly_deg = 1) {
  # Extract data
  df <- data.frame(
    force = profile_data[[force]],
    velocity = profile_data[[velocity]]
  )

  bodyweight <- profile_data$bodyweight[1]

  # model
  get_fv_model <- function(df) {
    tryCatch(
      {
        stats::lm(velocity ~ stats::poly(force, poly_deg), df)
      },
      error = function(cond) {
        # message(paste("Error when creating linear model for", force, "~", velocity, "profile"))
        # message("Here's the original error message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      },
      warning = function(cond) {
        # message(paste("Error when creating linear model for", force, "~", velocity, "profile"))
        # message("Here's the original warning message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      }
    )
  }

  profile_model <- get_fv_model(df)

  if (is.na(profile_model[1])) {
    return(list(
      F0 = NA,
      F0_rel = NA,
      V0 = NA,
      Pmax = NA,
      Pmax_rel = NA,
      Sfv = NA,
      Sfv_rel = NA
    ))
  }


  # Get V0
  get_velocity_0 <- function(profile_model) {
    tryCatch(
      {
        stats::predict(profile_model, newdata = data.frame(force = 0))
      },
      error = function(cond) {
        # message(paste("Error when finding V0 for", force, "~", velocity, "profile"))
        # message("Here's the original error message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      },
      warning = function(cond) {
        # message(paste("Warning when finding V0 for", force, "~", velocity, "profile"))
        # message("Here's the original warning message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      }
    )
  }

  velocity_0 <- get_velocity_0(profile_model)[[1]]

  if (is.na(velocity_0)) {
    return(list(
      F0 = NA,
      F0_rel = NA,
      V0 = NA,
      Pmax = NA,
      Pmax_rel = NA,
      Sfv = NA,
      Sfv_rel = NA
    ))
  }

  # get F0
  get_force_0 <- function(profile_model) {
    tryCatch(
      {
        stats::uniroot(
          function(x) {
            stats::predict(profile_model, newdata = data.frame(force = x))
          },
          interval = c(0, 10 * max(profile_data[[force]]))
        )
      },
      error = function(cond) {
        # message(paste("Error when finding F0 for", force, "~", velocity, "profile"))
        # message("Here's the original error message:")
        # message(cond)
        # message("\nReturning 0")
        return(list(root = NA))
      },
      warning = function(cond) {
        # message(paste("Warning when finding F0 for", force, "~", velocity, "profile"))
        # message("Here's the original warning message:")
        # message(cond)
        # message("\nReturning 0")
        return(list(root = NA))
      }
    )
  }
  force_0 <- get_force_0(profile_model)$root[[1]]

  if (is.na(force_0)) {
    return(list(
      F0 = NA,
      F0_rel = NA,
      V0 = NA,
      Pmax = NA,
      Pmax_rel = NA,
      Sfv = NA,
      Sfv_rel = NA
    ))
  }

  # get max Power
  power_max <- get_max_power(force_0, velocity_0)

  # Get slope
  slope <- get_slope(force_0, velocity_0)
  slope_rel <- get_slope(force_0 / bodyweight, velocity_0)

  return(list(
    F0 = force_0,
    F0_rel = force_0 / bodyweight,
    V0 = velocity_0,
    Pmax = power_max,
    Pmax_rel = power_max / bodyweight,
    Sfv = slope,
    Sfv_rel = slope_rel
  ))
}

#' Get Power-x profile
#'
#' Applied polynomial linear regression model using \code{power} and \code{x_var} columns from the \code{profile_data} and
#'     returns calculated \code{Pmax} and \code{Pmax_location} in \code{x_var}
#' @param profile_data Data frame returned from \code{\link{vj_profile}} function
#' @param power Character string. Name of the column in the \code{profile_data}. Default is "mean_power"
#' @param x_var Character string. Name of the column in the \code{profile_data}. Default is "mean_GRF_over_distance"
#' @param poly_deg Integer. Number of polynomial degrees. Forwarded to \code{\link[stats]{poly}} function. Default is 2
#' @return List with calculated \code{Pmax} and \code{Pmax_location}
#' @export
#' @examples
#' fv_profile <- vj_profile(mass = 75)
#' get_power_profile(fv_profile)
get_power_profile <- function(profile_data, power = "mean_power", x_var = "mean_GRF_over_distance", poly_deg = 2) {
  # Extract data
  df <- data.frame(
    power = profile_data[[power]],
    x_var = profile_data[[x_var]]
  )

  bodyweight <- profile_data$bodyweight[1]

  # model
  get_fv_model <- function(df) {
    tryCatch(
      {
        stats::lm(power ~ stats::poly(x_var, poly_deg), df)
      },
      error = function(cond) {
        # message(paste("Error when creating linear model for", power, "~", x_var, "profile"))
        # message("Here's the original error message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      },
      warning = function(cond) {
        # message(paste("Error when creating linear model for", power, "~", x_var, "profile"))
        # message("Here's the original warning message:")
        # message(cond)
        # message("\nReturning 0")
        return(NA)
      }
    )
  }

  profile_model <- get_fv_model(df)

  if (is.na(profile_model[1])) {
    return(list(
      Pmax = NA,
      Pmax_rel = NA,
      Pmax_location = NA
    ))
  }


  # get max Power
  get_max_power <- function(profile_model) {
    tryCatch(
      {
        stats::optimize(
          function(x) {
            stats::predict(profile_model, newdata = data.frame(x_var = x))
          },
          interval = c(0, 100 * max(profile_data[[x_var]])),
          maximum = TRUE
        )
      },
      error = function(cond) {
        # essage(paste("Error when finding Pmax for", power, "~", x_var, "profile"))
        # message("Here's the original error message:")
        # message(cond)
        # message("\nReturning 0")
        return(list(objective = NA, maximum = NA))
      },
      warning = function(cond) {
        # message(paste("Warning when finding Pmax for", power, "~", x_var, "profile"))
        # message("Here's the original warning message:")
        # message(cond)
        # message("\nReturning 0")
        return(list(objective = NA, maximum = NA))
      }
    )
  }

  power_max_optim <- get_max_power(profile_model)

  power_max <- power_max_optim$objective[[1]]

  # Get max Power location
  power_max_location <- power_max_optim$maximum[[1]]

  return(list(
    Pmax = power_max,
    Pmax_rel = power_max / bodyweight,
    Pmax_location = power_max_location
  ))
}

#' Get All Profiles
#'
#' Generates all Force-Velocity profiles and stores them in the list and data.frame
#' @param profile_data Data frame returned from \code{\link{vj_profile}} function
#' @return List with two elements: list of profiles and long data frame
#' @export
#' @examples
#' fv_profile <- vj_profile(mass = 75)
#' all_profiles <- get_all_profiles(fv_profile)
#' all_profiles$data_frame
get_all_profiles <- function(profile_data) {

  # Mean Force ~ Mean Velocity Profile
  profile_mean_FV <- get_FV_profile(
    profile_data,
    force = "mean_GRF_over_distance",
    velocity = "mean_velocity"
  )

  # Mean Power ~ Mean Force Profile
  profile_mean_power <- get_power_profile(
    profile_data,
    power = "mean_power",
    x_var = "mean_GRF_over_distance"
  )
  profile_mean_power$F0_perc <- profile_mean_power$Pmax_location / profile_mean_FV$F0

  # Peak Force ~ Peak Velocity Profile
  profile_peak_FV <- get_FV_profile(
    profile_data,
    force = "peak_GRF",
    velocity = "peak_velocity"
  )

  # Peak Power ~ Peak Force Profile
  profile_peak_power <- get_power_profile(
    profile_data,
    power = "peak_power",
    x_var = "peak_GRF"
  )
  profile_peak_power$F0_perc <- profile_peak_power$Pmax_location / profile_peak_FV$F0

  # Load ~ Take Off Velocity
  profile_load_take_off_velocity <- get_FV_profile(
    profile_data,
    force = "mass",
    velocity = "take_off_velocity"
  )
  profile_load_take_off_velocity$L0 <- profile_load_take_off_velocity$F0
  profile_load_take_off_velocity$L0_rel <- profile_load_take_off_velocity$F0_rel
  profile_load_take_off_velocity$Imax <- profile_load_take_off_velocity$Pmax
  profile_load_take_off_velocity$Imax_rel <- profile_load_take_off_velocity$Pmax_rel
  profile_load_take_off_velocity$Slv <- profile_load_take_off_velocity$Sfv
  profile_load_take_off_velocity$Slv_rel <- profile_load_take_off_velocity$Sfv_rel
  profile_load_take_off_velocity$F0 <- NULL
  profile_load_take_off_velocity$F0_rel <- NULL
  profile_load_take_off_velocity$Pmax <- NULL
  profile_load_take_off_velocity$Pmax_rel <- NULL
  profile_load_take_off_velocity$Sfv <- NULL
  profile_load_take_off_velocity$Sfv_rel <- NULL

  # Impulse ~ Load
  profile_load_impulse <- get_power_profile(
    profile_data,
    power = "impulse",
    x_var = "mass"
  )
  profile_load_impulse$Imax <- profile_load_impulse$Pmax
  profile_load_impulse$Imax_rel <- profile_load_impulse$Pmax_rel
  profile_load_impulse$Imax_location <- profile_load_impulse$Pmax_location
  profile_load_impulse$L0_perc <- profile_load_impulse$Imax_location / profile_load_take_off_velocity$L0
  profile_load_impulse$Pmax <- NULL
  profile_load_impulse$Pmax_rel <- NULL
  profile_load_impulse$Pmax_location <- NULL

  # Bind all profiles
  profiles_list <- list(
    profile_mean_FV = profile_mean_FV,
    profile_mean_power = profile_mean_power,
    profile_peak_FV = profile_peak_FV,
    profile_peak_power = profile_peak_power,
    profile_load_take_off_velocity = profile_load_take_off_velocity,
    profile_load_impulse = profile_load_impulse
  )

  profiles_df <- dplyr::bind_rows(profiles_list, .id = "profile")
  profiles_df <- tidyr::gather(profiles_df, "metric", "value", -1, na.rm = TRUE)
  profiles_df$profile <- factor(
    profiles_df$profile,
    levels = c(
      "profile_mean_FV",
      "profile_mean_power",
      "profile_peak_FV",
      "profile_peak_power",
      "profile_load_take_off_velocity",
      "profile_load_impulse"
    ),
    labels = c(
      "Mean Force ~ Mean Velocity Profile",
      "Mean Power ~ Mean Force Profile",
      "Peak Force ~ Peak Velocity Profile",
      "Peak Power ~ Peak Force Profile",
      "Load ~ Take-off Velocity Profile",
      "Impulse ~ Load Profile"
    )
  )

  # Sort data frame
  profiles_df <- profiles_df[order(profiles_df$profile), ]

  return(list(
    list = profiles_list,
    data_frame = profiles_df
  ))
}

#' Make Load Profile
#'
#' \code{make_load_profile} generates Load-TOV (take-off velocity) profile from squat jump data. When both \code{aerial_time} and
#'     \code{height} are forwarded to the function, only \code{height} is used to estimate TOV.
#'
#' @param bodyweight Numeric value
#' @param external_load Numeric vector
#' @param height Numeric vector
#' @param aerial_time Numeric vector
#' @param gravity_const Numeric vector. Default 9.81
#' @param plot TRUE/FALSE. Default is TRUE
#' @return List of L-TOV profile parameters
#' @export
#' @examples
#' require(tidyverse)
#'
#' data("testing_data")
#'
#' with(
#'   filter(testing_data, athlete == "Jack"),
#'   make_load_profile(
#'     bodyweight = bodyweight,
#'     external_load = external_load,
#'     aerial_time = aerial_time,
#'     plot = TRUE
#'   )
#' )
make_load_profile <- function(bodyweight,
                              external_load,
                              height,
                              aerial_time,
                              gravity_const = 9.81,
                              plot = TRUE) {

  # Check if both height and aerial_time are used
  if (!missing(height) & !missing(aerial_time)) {
    warning("Please use either height or aerial time as a parameter. Using height to model", call. = FALSE)
  }

  # If height missing, use aerial time to create it
  if (missing(height)) {
    height <- get_height_from_aerial_time(aerial_time, gravity_const)
  }

  # Get take-off velocity
  TOV <- sqrt(2 * gravity_const * height)

  df <- data.frame(
    bodyweight = bodyweight,
    load = bodyweight + external_load,
    TOV = TOV
  )

  LTOV_profile <- get_FV_profile(df, force = "load", velocity = "TOV")

  # Get model fit metrics
  predicted_TOV <- LTOV_profile$V0 + df$load / LTOV_profile$Sfv
  RSE <- sqrt(sum((predicted_TOV - TOV)^2) / (length(TOV) - 2))
  R_squared <- stats::cor(predicted_TOV, TOV)^2


  if (plot) {
    plot_data <- data.frame(
      group = rep(1:2, each = 2),
      x = c(LTOV_profile$F0, 0),
      y = c(0, LTOV_profile$V0)
    )

    gg <- ggplot2::ggplot() +
      cowplot::theme_cowplot(8) +

      ggplot2::geom_point(
        ggplot2::aes(
          x = df$load,
          y = df$TOV
        ),
        alpha = 0.8
      ) +

      ggplot2::geom_line(
        ggplot2::aes(
          x = plot_data$x,
          y = plot_data$y
        ),
        alpha = 0.8
      ) +

      ggplot2::xlab("Load [kg]") +
      ggplot2::ylab("Take-off Velocity [m/s]") +
      ggplot2::ggtitle(
        "Load - TOV profile",
        paste(
          "L0 = ", round(LTOV_profile$F0, 0), "kg",
          "; V0 = ", round(LTOV_profile$V0, 2), "m/s",
          "; Sfv = ", round(LTOV_profile$Sfv, 0),
          "; RSE = ", round(RSE, 2), "m/s",
          "; R2 = ", round(R_squared, 2),
          sep = ""
        )
      )
    plot(gg)
  }

  return(list(
    L0 = LTOV_profile$F0,
    L0_rel = LTOV_profile$F0_rel,
    TOV0 = LTOV_profile$V0,
    Sltv = LTOV_profile$Sfv,
    Sltv_rel = LTOV_profile$Sfv_rel,
    RSE = RSE,
    R_squared = R_squared
  ))
}
