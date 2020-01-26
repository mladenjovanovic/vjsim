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
#'   probe_func = function(...){fgen_get_output(...)}, aggregate = "raw",
#'   mass = 70, max_force = 5000
#' )
#'
#' ggplot(
#' fgen_probe_data,
#'  aes(
#'    x = change_ratio,
#'    y = kinetics.ground_reaction_force,
#'    color = probing
#'   )
#' ) +
#' geom_line()

get_probing_data <- function(args_list,
                             probe_func = function(...){},
                             change_ratio = seq(0.8, 1.2, length.out = 100),
                             aggregate = "raw",
                             ...) {

  #do.call(pred_func, c(args_list, ...))
  #do.call(expand.grid, c(args_list, list(probe = probe)))
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
    probing_data[seq((i-1)*change_number + 1, i * change_number), 2 + i] <- change_ratio * unlist(args_list)[[i]]
  }

  # Get output data
  out_data <- do.call(probe_func, do.call(list, probing_data[-c(1:2)]))

  init_out_data <- do.call(probe_func, c(args_list, ...))

  # Convert to data frame
  out_data <- do.call(data.frame, out_data)
  init_out_data <- do.call(data.frame, init_out_data)

  # Repeat init_out_data so it has equal rows to out_data
  init_out_data[seq(1, nrow(out_data)),] <- init_out_data[1 ,]


  if (aggregate == "diff") {
    out_data <-  out_data -  init_out_data
  }

  if (aggregate == "ratio") {
    out_data <-  out_data /  init_out_data
  }

  # Bind the output
  cbind(probing_data,  out_data)
}

#' Probe Force Generator
#'
#' @param current_time Numeric value. Initial system state whose change is probed
#' @param current_distance Numeric value. Initial system state whose change is probed
#' @param current_velocity Numeric value. Initial system state whose change is probed
#' @param change_ratio Numeric vector indicating probing change ratios
#' @param aggregate How should \code{link{fgen_get_output}} output be aggregated?
#'     Default is "raw". Other options involve "ratio" and "diff" which use initial
#'     output values
#' @param ... Extra argument forwarded to \code{link{fgen_get_output}}
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
#' ggplot(plot_data, aes(x = change_ratio, y = value,  color = probing)) +
#'   geom_line() +
#'   facet_wrap(~variable, scales = "free_y") +
#'   xlab("Current state change") +
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
    probe_func = function(...){fgen_get_output(...)},
    aggregate = aggregate,
    ...
  )

 return(fgen_probe_data)
}



