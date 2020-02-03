#' Get Probing Data
#'
#' @param args_list Named list with initial arguments to be passed to \code{probe_func}
#' @param probe_func Function that returns a list of outputs
#' @param change_ratio Numeric vector indicating the probing initial arguments in \code{args_list}
#' @param aggregate String. Indicates how should output from \code{probe_func} be aggregated.
#'     Default is "raw". Other options involve "diff" and "ratio". To calculate "diff" and
#'     "ratio" aggregate, \code{probe_func} output using the initial argument values in
#'     \code{args_list} is used
#' @param ... Extra arguments for \code{probe_func} that are not probed using \code{change_ratio}
#' @return Data frame
#' @export
#' @examples
#' require(ggplot2)
#' fgen_probe_data <- get_probing_data(
#'   args_list = list(current_time = 0.3, current_distance = 0.1, current_velocity = 0),
#'   probe_func = function(...) {
#'     fgen_get_output(...)
#'   }, aggregate = "raw",
#'   mass = 70, max_force = 5000
#' )
#'
#' ggplot(
#'   fgen_probe_data,
#'   aes(
#'     x = change_ratio,
#'     y = kinetics.ground_reaction_force,
#'     color = probing
#'   )
#' ) +
#'   geom_line()
get_probing_data <- function(args_list,
                             probe_func = function(...) {},
                             change_ratio = seq(0.8, 1.2, length.out = 100),
                             aggregate = "raw",
                             ...) {

  # do.call(pred_func, c(args_list, ...))
  # do.call(expand.grid, c(args_list, list(probe = probe)))
  args_names <- names(args_list)
  args_number <- length(args_list)

  change_number <- length(change_ratio)

  # Generate combinations
  probing_data <- do.call(
    expand.grid,
    c(
      list(change_ratio = change_ratio),
      list(probing = args_names),
      args_list,
      ...
    )
  )

  # Modify only one args while keeping others the same
  for (i in seq(1, args_number)) {
    probing_data[seq((i - 1) * change_number + 1, i * change_number), 2 + i] <- change_ratio * unlist(args_list)[[i]]
  }

  # Get output data
  out_data <- do.call(probe_func, do.call(list, probing_data[-c(1:2)]))

  init_out_data <- do.call(probe_func, c(args_list, ...))

  # Convert to data frame
  out_data <- do.call(data.frame, out_data)
  init_out_data <- do.call(data.frame, init_out_data)

  # Repeat init_out_data so it has equal rows to out_data
  init_out_data[seq(1, nrow(out_data)), ] <- init_out_data[1, ]


  if (aggregate == "diff") {
    out_data <- out_data - init_out_data
  }

  if (aggregate == "ratio") {
    out_data <- out_data / init_out_data
  }

  # Bind the output
  cbind(probing_data, out_data)
}

#' Probe Force Generator
#'
#' @param current_time Numeric value. Initial system state whose change is probed
#' @param current_distance Numeric value. Initial system state whose change is probed
#' @param current_velocity Numeric value. Initial system state whose change is probed
#' @param change_ratio Numeric vector indicating probing change ratios
#' @param aggregate How should \code{\link{fgen_get_output}} output be aggregated?
#'     Default is "raw". Other options involve "ratio" and "diff" which use initial
#'     output values
#' @param ... Extra argument forwarded to \code{\link{fgen_get_output}}
#' @return Probing data frame
#' @export
#' @examples
#' require(tidyverse)
#'
#' fgen_probe_data <- probe_fgen(
#'   current_time = 0.3,
#'   current_distance = 0.3,
#'   current_velocity = 1
#' )
#'
#' plot_data <- gather(fgen_probe_data, key = "variable", value = "value", -(1:2))
#'
#' ggplot(plot_data, aes(x = change_ratio, y = value, color = probing)) +
#'   geom_line() +
#'   facet_wrap(~variable, scales = "free_y") +
#'   xlab("Normalized parameter change") +
#'   ylab(NULL)
probe_fgen <- function(current_time = 0,
                       current_distance = 0,
                       current_velocity = 0,
                       change_ratio = seq(0.8, 1.2, length.out = 100),
                       aggregate = "raw",
                       ...) {
  fgen_probe_data <- get_probing_data(
    args_list = list(
      current_time = current_time,
      current_distance = current_distance,
      current_velocity = current_velocity
    ),
    probe_func = function(...) {
      fgen_get_output(...)
    },
    aggregate = aggregate,
    change_ratio = change_ratio,
    ...
  )

  return(fgen_probe_data)
}

#' Probe Vertical Jump
#'
#' \code{probe_vj} simulates the vertical jump, but estimate which parameter brings biggest change. This is done
#'     by keeping all parameters at initial value, while changing only one parameter. This is then repeated for
#'     all parameters. This way we can answer by changing what parameter for standardize change (\code{change_ratio})
#'     yield biggest change in summary metric (e.g. jump height)
#' @param mass Numeric value. Initial parameter value to be changed using \code{change_ratio}.
#' @param push_off_distance Numeric value. Initial parameter value to be changed using \code{change_ratio}
#' @param max_force Numeric value. Initial parameter value to be changed using \code{change_ratio}
#' @param max_velocity Numeric value. Initial parameter value to be changed using \code{change_ratio}
#' @param time_to_max_activation Numeric value. Initial parameter value to be changed using \code{change_ratio}
#' @param change_ratio Numeric vector indicating probing change ratios
#' @param aggregate How should \code{\link{vj_simulate}} output be aggregated?
#'     Default is "raw". Other options involve "ratio" and "diff" which use initial
#'     output values
#' @param ... Extra argument forwarded to \code{\link{vj_simulate}}
#' @return Probing data frame

#' @export
#' @examples
#' require(tidyverse)
#'
#' vj_probe_data <- probe_vj(
#'   mass = 75,
#'   max_force = 3000,
#'   max_velocity = 3,
#'   time_to_max_activation = 0.3,
#'   time_step = 0.001
#' )
#'
#' # Invert for mass and time_to_max_activation
#' vj_probe_data$change_ratio <- ifelse(
#'   vj_probe_data$probing == "time_to_max_activation",
#'   1 / vj_probe_data$change_ratio,
#'   vj_probe_data$change_ratio
#' )
#'
#' vj_probe_data$change_ratio <- ifelse(
#'   vj_probe_data$probing == "mass",
#'   1 / vj_probe_data$change_ratio,
#'   vj_probe_data$change_ratio
#' )
#'
#'
#' plot_data <- gather(vj_probe_data, key = "variable", value = "value", -(1:9)) %>%
#'   filter(variable %in% c(
#'     "height",
#'     "take_off_time",
#'     "mean_velocity",
#'     "peak_velocity",
#'     "take_off_velocity",
#'     "mean_GRF_over_distance",
#'     "mean_GRF_over_time",
#'     "peak_GRF",
#'     "peak_power",
#'     "mean_power",
#'     "peak_RFD",
#'     "peak_RPD"
#'   ))
#'
#' plot_data$reverse <- plot_data$probing %in% c("mass", "time_to_max_activation")
#'
#' ggplot(plot_data, aes(x = change_ratio, y = value, color = probing, linetype = reverse)) +
#'   theme_minimal() +
#'   geom_line() +
#'   facet_wrap(~variable, scales = "free_y") +
#'   xlab("Normalized parameter change") +
#'   ylab(NULL) +
#'   scale_color_manual(values = c(
#'     "mass" = "#4D4D4D",
#'     "max_force" = "#5DA5DA",
#'     "max_velocity" =  "#FAA43A",
#'     "push_off_distance" = "#60BD68",
#'     "time_to_max_activation" = "#B276B2"))
probe_vj <- function(mass = 75,
                     push_off_distance = 0.4,
                     max_force = 3000,
                     max_velocity = 4,
                     time_to_max_activation = 0.3,
                  change_ratio = seq(0.9, 1.1, length.out = 3),
                  aggregate = "raw",
                  ...) {
  fgen_probe_data <- get_probing_data(
    args_list = list(
      mass = mass,
      push_off_distance = push_off_distance,
      max_force = max_force,
      max_velocity = max_velocity,
      time_to_max_activation = time_to_max_activation
    ),
    probe_func = function(...) {
      # Convert to data frame
      df <- as.data.frame(list(...))
      n_params <- nrow(df)

      out.df <- list(n_params)

      for(i in seq(1, n_params)) {
        params <- df[i,]
        out <- do.call(vj_simulate, params)
        out.df[[i]] <- out$summary[, -c(1, 2, 3, 4, 5, 7, 8)]
      }
    return(do.call(list, do.call(rbind, out.df)))
    },
    aggregate = aggregate,
    change_ratio = change_ratio,
    save_trace = FALSE,
    ...
  )

  return(fgen_probe_data)
}
