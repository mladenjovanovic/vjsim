#' Vertical Jump Simulation
#'
#' Simulates vertical jump using Runge-Kutta method
#'
#' @param mass Numeric value. Default is 75kg
#' @param weight Numeric value. Deafult is \code{mass} * \code{gravity_const}
#' @param push_off_distance Numeric value. Default is 0.4m
#' @param gravity_const Numeric value. Default is 9.81
#' @param time_step Numeric value. Time step used in simulation. Default is 0.01
#' @param save_trace Logical. Default is TRUE
#' @param fgen_func Function used to represent Force Generator. Defauls it \code{link{fgen_get_output}}.
#' @param iter Logical. Default is FALSE
#' @param max_iter Numeric value. Defaut value 1000
#' @param ... Forwarded to \code{fgen_func}
#' @return List object with \code{summary} data frame and \code{trace} data frame
#' @export
#' @examples
#' vertical_jump <- vj_simulate(
#'   mass = 85,
#'   push_off_distance = 0.4,
#'   time_step = 0.001
#' )
#'
#' round(t(vertical_jump$summary), 3)
#'
#' plot(
#'   x = vertical_jump$trace$distance,
#'   y = vertical_jump$trace$velocity,
#'   type = "l"
#' )
#'
#' plot(
#'   x = vertical_jump$trace$time,
#'   y = vertical_jump$trace$ground_reaction_force,
#'   type = "l"
#' )
#'
#' plot(
#'   x = vertical_jump$trace$distance,
#'   y = vertical_jump$trace$ground_reaction_force,
#'   type = "l"
#' )
vj_simulate <- function( # system constrains
                        mass = 75,
                        weight = mass * gravity_const,
                        push_off_distance = 0.4,
                        gravity_const = 9.81,

                        # simulation parameters
                        time_step = 0.01,
                        save_trace = TRUE,

                        # Force generator function
                        fgen_func = fgen_get_output,

                        iter = FALSE,
                        max_iter = 1000,
                        ...) {

  # Init variables
  current_time <- 0
  next_time <- 0

  current_distance <- 0
  next_distance <- 0

  current_velocity <- 0
  next_velocity <- 0

  current_acceleration <- 0

  current_RFD <- NA
  current_RPD <- NA

  # Used to calculate RFD
  previous_GRF <- 0

  # Used to calculate RPD
  previous_power <- 0

  # List for saving kinetics trace
  trace_data <- list(max_iter)
  trace_index <- 1

  # --------------------------
  # summary data

  # Ground Reaction Force
  summary_peak_GRF <- 0
  summary_peak_GRF_time <- 0
  summary_peak_GRF_distance <- 0

  # Peak Velocity
  summary_peak_velocity <- 0
  summary_peak_velocity_time <- 0
  summary_peak_velocity_distance <- 0

  # Rate of Force Development
  summary_RFD <- 0
  summary_peak_RFD <- 0
  summary_peak_RFD_time <- 0
  summary_peak_RFD_distance <- 0

  # Peak Power
  summary_peak_power <- 0
  summary_peak_power_time <- 0
  summary_peak_power_distance <- 0

  # Rate of Power Development
  summary_RPD <- 0
  summary_peak_RPD <- 0
  summary_peak_RPD_time <- 0
  summary_peak_RPD_distance <- 0

  # ----------------------------------------
  # Simulation loop
  while (next_distance <= push_off_distance) {

    # Do the safety check
    if (trace_index > max_iter) {
      message("Maximal iterations reached. Returning NULL. Check the simulation parameters")
      return(NULL)
    }

    # Update kinematic variables
    current_time <- next_time
    current_distance <- next_distance
    current_velocity <- next_velocity

    if (iter) {
      message(paste("t=", round(current_time, 3), " d=", round(current_distance, 3)))
    }

    # Get Force Generator output
    fgen_output <- fgen_func(
      # Forward current state
      current_time = current_time,
      current_distance = current_distance,
      current_velocity = current_velocity,

      # system constrains
      mass = mass,
      weight = weight,
      push_off_distance = push_off_distance,
      gravity_const = gravity_const,

      # Forward extra arguments (Force Generator parameters)
      ...
    )

    # Get kinetic output
    # These must be returned by `fgen_func`
    ground_reaction_force <- fgen_output$kinetics$ground_reaction_force
    propulsive_force <- fgen_output$kinetics$propulsive_force
    current_acceleration <- fgen_output$kinetics$acceleration

    # -------------------------------------------
    # Calculate the New state of the system using
    # Runge-Kutta 4 method
    dx1 <- current_velocity
    dv1 <- current_acceleration

    #---
    dx2 <- current_velocity + (0.5 * time_step * dv1)
    dv2 <- fgen_func(
      # Forward current state
      current_time = current_time + (0.5 * time_step),
      current_distance = current_distance + (0.5 * time_step * dx1),
      current_velocity = current_velocity + (0.5 * time_step * dv1),

      # system constrains
      mass = mass,
      weight = weight,
      push_off_distance = push_off_distance,
      gravity_const = gravity_const,

      # Forward extra arguments (Force Generator parameters)
      ...
    )
    dv2 <- dv2$kinetics$acceleration

    # ---
    dx3 <- current_velocity + (0.5 * time_step * dv2)
    dv3 <- fgen_func(
      # Forward current state
      current_time = current_time + (0.5 * time_step),
      current_distance = current_distance + (0.5 * time_step * dx2),
      current_velocity = current_velocity + (0.5 * time_step * dv2),

      # system constrains
      mass = mass,
      weight = weight,
      push_off_distance = push_off_distance,
      gravity_const = gravity_const,

      # Forward extra arguments (Force Generator parameters)
      ...
    )
    dv3 <- dv3$kinetics$acceleration

    # ---
    dx4 <- current_velocity + (time_step * dv3)
    dv4 <- fgen_func(
      # Forward current state
      current_time = current_time + (time_step),
      current_distance = current_distance + (0.5 * time_step * dx3),
      current_velocity = current_velocity + (0.5 * time_step * dv3),

      # system constrains
      mass = mass,
      weight = weight,
      push_off_distance = push_off_distance,
      gravity_const = gravity_const,

      # Forward extra arguments (Force Generator parameters)
      ...
    )
    dv4 <- dv4$kinetics$acceleration

    dx <- (dx1 + 2 * (dx2 + dx3) + dx4) / 6
    dv <- (dv1 + 2 * (dv2 + dv3) + dv4) / 6

    # Update the new system state
    next_distance <- current_distance + dx * time_step
    next_velocity <- current_velocity + dv * time_step

    # --------------------------------------------
    # Summary metrics

    if (current_velocity > summary_peak_velocity) {
      summary_peak_velocity <- current_velocity
      summary_peak_velocity_distance <- current_distance
      summary_peak_velocity_time <- current_time
    }

    if (ground_reaction_force > summary_peak_GRF) {
      summary_peak_GRF <- ground_reaction_force
      summary_peak_GRF_distance <- current_distance
      summary_peak_GRF_time <- current_time
    }

    current_power <- current_velocity * ground_reaction_force

    if (current_power > summary_peak_power) {
      summary_peak_power <- current_power
      summary_peak_power_distance <- current_distance
      summary_peak_power_time <- current_time
    }

    if (trace_index > 1) {
      current_RFD <- (ground_reaction_force - previous_GRF) / time_step

      if (current_RFD > summary_peak_RFD) {
        summary_peak_RFD <- current_RFD
        summary_peak_RFD_distance <- current_distance
        summary_peak_RFD_time <- current_time
      }

      current_RPD <- (current_power - previous_power) / time_step

      if (current_RPD > summary_peak_RPD) {
        summary_peak_RPD <- current_RPD
        summary_peak_RPD_distance <- current_distance
        summary_peak_RPD_time <- current_time
      }
    }

    # Add RFD and RPD to trace
    fgen_output$kinetics$RFD <- current_RFD
    fgen_output$kinetics$RPD <- current_RPD

    # --------------------------------------------
    # Save trace
    if (save_trace) {
      names(fgen_output) <- NULL
      trace_data[[trace_index]] <- as.data.frame(fgen_output)
    }

    # --------------------------------------------
    # Update the timer
    next_time <- current_time + time_step

    # update trace index
    trace_index <- trace_index + 1

    # Update previous_GRF
    previous_GRF <- ground_reaction_force

    # Update previous_power
    previous_power <- current_power
  } # Main loop finished


  # Summary metrics
  summary_data <- data.frame(
    mass = mass,
    weight = weight,
    push_off_distance = push_off_distance,
    gravity_const = gravity_const,

    current_distance = current_distance,
    current_time = current_time,
    current_ground_reaction_force = ground_reaction_force,
    current_propulsive_force = propulsive_force,

    take_off_velocity = current_velocity,

    height = get_height(
      take_off_velocity = current_velocity,
      gravity_const = gravity_const
    ),

    mean_GRF_over_distance = get_mean_force_over_distance(
      mass = mass,
      weight = weight,
      take_off_velocity = current_velocity,
      push_off_distance = current_distance
    ),

    mean_GRF_over_time = get_mean_force_over_time(
      mass = mass,
      weight = weight,
      take_off_velocity = current_velocity,
      time_taken = current_time
    ),

    mean_velocity = current_distance / current_time,

    work_done = get_work(
      mass = mass,
      weight = weight,
      take_off_velocity = current_velocity,
      push_off_distance = current_distance
    ),

    impulse = get_impuse(
      mass = mass,
      take_off_velocity = current_velocity
    ),

    mean_power = get_mean_power(
      mass = mass,
      weight = weight,
      take_off_velocity = current_velocity,
      push_off_distance = current_distance,
      time_taken = current_time
    ),

    mean_RFD = summary_peak_GRF / summary_peak_GRF_time,
    mean_RPD = summary_peak_power / summary_peak_power_time,

    peak_GRF = summary_peak_GRF,
    peak_GRF_time = summary_peak_GRF_time,
    peak_GRF_distance = summary_peak_GRF_distance,

    peak_velocity = summary_peak_velocity,
    peak_velocity_time = summary_peak_velocity_time,
    peak_velocity_distance = summary_peak_velocity_distance,

    peak_power = summary_peak_power,
    peak_power_distance = summary_peak_power_distance,
    peak_power_time = summary_peak_power_time,

    peak_RFD = summary_peak_RFD,
    peak_RFD_distance = summary_peak_RFD_distance,
    peak_RFD_time = summary_peak_RFD_time,

    peak_RPD = summary_peak_RPD,
    peak_RPD_distance = summary_peak_RPD_distance,
    peak_RPD_time = summary_peak_RPD_time
  )

  names(trace_data) <- NULL

  return(
    list(
      summary = summary_data,
      trace =  do.call(rbind, trace_data)
    )
  )
}
