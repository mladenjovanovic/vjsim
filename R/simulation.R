#' Vertical Jump Simulation
#' @param mass
#' @param weight
#' @param push_off_distance
#' @param gravity_const Numeric value. Default is 9.81
#' @param time_step
#' @param save_trace
#' @param fgen_func
#' @param iter
#' @param max_iter Numeric value. Defaut value 1000
#' @param ... Forwarded to \code{fgen_func}
#' @return
#' @export
#' @examples
#'
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

  # List for saving kinetics trace
  trace_data <- list(max_iter)
  trace_index <- 1

  # --------------------------
  # summary data

  # Ground Reaction Force
  summary_peak_GRF <- 0
  summary_peak_GRF_time <- 0
  summary_peak_GRF_distance <- 0

  # Explosive Strength Index
  summary_ESI <- 0
  summary_ESI_time <- 0
  summary_ESI_distance <- 0

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
      message(paste("t=", round(current_time,3), " d=", round(current_distance, 3)))
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

    # --------------------------------------------
    # Save trace
    if (save_trace) {
       trace_data[[trace_index]] <- as.data.frame(fgen_output)
    }

    # --------------------------------------------
    # Update the timer
    next_time <- current_time + time_step

    # update trace index
    trace_index <- trace_index + 1
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

     peak_velocity = summary_peak_velocity,
     peak_velocity_time = summary_peak_velocity_time,
     peak_velocity_distance = summary_peak_velocity_distance

  )


  return(
     list(
        summary = summary_data,
        trace = do.call(rbind, trace_data)
        )
  )
}
