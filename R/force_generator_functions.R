#' Force Length Relationship
#'
#' @param current_distance Numeric vector
#' @param start_force Numeric value. Indicates starting percentage. Allowed range from 0.5 to 1. Default is 0.8
#' @param end_force Numeric value. Indicates ending percentage. Allowed range from 0.5 to 1. Default is 0
#' @param threshold Numeric value. Indicates threshold where the line starts to break. Use values from 0 to 1, where
#'     1 is equal to \code{push_off_distance} position. Allowed range from 0.8 to 1. Default is 0.9
#' @param push_off_distance Numeric value. Indicates the push off distance/length. Default is 0.4.
#' @param ... Used to allow different parameters to be passes without error
#' @return Numeric vector with values from 0 to 1, indicating percentage of Force at particular \code{current_distance}
#' @export
#' @examples
#' x <- seq(0, 0.5, length.out = 1000)
#'
#' y1 <- fgen_force_length(
#'   current_distance = x,
#'   start_force = 0.6,
#'   threshold = 0.90,
#'   end = 0
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_force_length(
#'   current_distance = x,
#'   start_force = 0.8,
#'   threshold = 0.95,
#'   end = 0
#' )
#'
#' lines(x, y2, col = "red")
fgen_force_length <- function(current_distance,
                              start_force = 0.8,
                              end_force = 0,
                              threshold = 0.9,
                              push_off_distance = 0.4,
                              ...) {
  if (any(current_distance < 0))
    stop("Current distance cannot be below zero.", call. = FALSE)

  if (start_force > 1 || start_force < 0.5)
    stop("Start force needs to be within 0.5 - 1.")

  if (end_force > 1 || end_force < 0)
    stop("End force needs to be within 0 - 1.")

  if (threshold > 1 || threshold < 0.8)
    stop("Threshold needs to be within 0.8 - 1.")

  current_distance <- (1 / push_off_distance) * current_distance
  y1 <- (1 - start_force) * sin((pi * current_distance) / (2 * threshold)) + start_force
  alpha <- (end_force - 1) / (threshold - 1)^2
  y2 <- alpha * (current_distance - threshold)^2 + 1

  force <- ifelse(current_distance < threshold, y1, y2)
  force <- ifelse(current_distance > 1, end_force, force)

  return(force)
}


#' Force Time Relationship
#' @param current_time Numeric vector
#' @param initial_force Numeric value. Default is 0
#' @param max_force Numeric value. Default is 1
#' @param time_to_max_force Numeric value. Time it takes to reach \code{max_force} from 0.
#'     This means that is \code{initial_force} is higher than 0, it will take less time
#'     to reack \code{max_force}. Default is 0.3s.
#' @param ... Used to allow different parameters to be passes without error
#' @return Numeric vector. Force level at \code{current_time}
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 1000)
#'
#' y1 <- fgen_force_time(
#'   current_time = x,
#'   initial_force = 0,
#'   max_force = 1000,
#'   time_to_max_force = 0.5
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_force_time(
#'   current_time = x,
#'   initial_force = 200,
#'   max_force = 1000,
#'   time_to_max_force = 0.5
#' )
#'
#' lines(x, y2, col = "red")
#'
#' y3 <- fgen_force_time(
#'   current_time = x,
#'   initial_force = 200,
#'   max_force = 1000,
#'   time_to_max_force = 0.3
#' )
#'
#' lines(x, y3, col = "blue")
fgen_force_time <- function(current_time,
                            initial_force = 0,
                            max_force = 3000,
                            time_to_max_force = 0.3,
                            ...) {

  if (any(current_time < 0))
    stop("Current time cannot be below zero.", call. = FALSE)

  # The case when system will go down, due weight higher than force generated
  if (any(current_time == 0 & max_force <= initial_force))
    stop("Unable to jump, since weight is higher than force generated", call. = FALSE)

  IES <- max_force / (time_to_max_force)
  time_to_max_force <- time_to_max_force * ((max_force - initial_force) / max_force)

  alpha <- (4 * IES) / (max_force - initial_force) * 2.5
  force <- (max_force - initial_force) / (1 + exp(-alpha * (current_time - time_to_max_force / 2))) + initial_force

  # Check
  if(length(max_force) == 1 & length(initial_force) == 1) {
    if(max_force < initial_force) {
      force <- rep(max_force, length(force))
    }
  } else {
  force <- ifelse(
    max_force < initial_force,
    max_force,
    force
  )
}
  return(force)
}


#' Force Velocity Relationhsip
#' @param current_velocity Numeric vector
#' @param max_force Numeric value. Default is 3000 Newtons
#' @param max_velocity Numeric value. Default is 4 m/s
#' @param ... Used to allow different parameters to be passes without error
#' @return Viscous force at \code{current_velocity}
#' @export
#' @examples
#' x <- seq(0, 3, length.out = 1000)
#'
#' y1 <- fgen_force_velocity(
#'   current_velocity = x,
#'   max_velocity = 3,
#'   max_force = 1200
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_force_velocity(
#'   current_velocity = x,
#'   max_velocity = 3,
#'   max_force = 1000
#' )
#'
#' lines(x, y2, col = "red")
fgen_force_velocity <- function(current_velocity,
                                max_force = 3000,
                                max_velocity = 4,
                                ...) {
  visc_factor <- max_force / max_velocity
  return(-current_velocity * visc_factor)
}

#' Get instanteous output of the Force Generator
#'
#' \code{fgen_get_output} returns acceleration and force data based on the current state
#'     of the system (\code{current_time}, mass, \code{current_velocity}, \code{current_distance}). Parameters in the \code{...} are forwarded to
#'     \code{\link{fgen_force_length}}, \code{\link{fgen_force_time}}, and \code{\link{fgen_force_velocity}}
#'     functions that make Force Generator characteristics.
#' @param mass Numeric value
#' @param current_time Numeric value
#' @param current_distance Numeric value
#' @param current_velocity Numeric value
#' @param max_force Numeric value
#' @param gravity_const Numeric value. Default is 9.81
#' @param ... Use to forward parameters to \code{\link{fgen_force_length}},
#'     \code{\link{fgen_force_time}}, and \code{\link{fgen_force_velocity}} functions
#' @return Data frame
#' @export
#' @examples
#' state <- expand.grid(
#' current_time = seq(0, 0.25, length.out = 1000),
#' current_distance = c(0.1, 0.15, 0.2, 0.25, 0.3),
#' current_velocity = 0)
#'
#' fgen_data <- fgen_get_output(
#'   max_force = 3000,
#'   mass = 75,
#'   current_time = state$current_time,
#'   current_distance = state$current_distance,
#'   current_velocity = state$current_velocity
#' )
fgen_get_output <- function(mass = 75,
                            current_time = 0,
                            current_distance = 0,
                            current_velocity = 0,
                            max_force = 3000,
                            gravity_const = 9.81,
                            ...) {
  weight <- gravity_const * mass

  # Get percent of maximal force based on the current position (distance)
  force_percentage <- fgen_force_length(
    current_distance = current_distance,
    ...
  )

  # Maximal force that can be generate at particular position
  force_potential <- max_force * force_percentage

  # Using maximal achievable force on the current position (force_potential), use
  # force-time relationship to model current force
  force_generated <- fgen_force_time(
    current_time = current_time,
    initial_force = weight, # Must be equal, since it is SJ (no counter-mov)
    max_force = force_potential,
    ...
  )

  # Viscous force
  force_viscous <- fgen_force_velocity(
    current_velocity = current_velocity, #* forcePercentage,
    max_force = max_force,
    ...
  )

  # Total force, acting on the object
  # Note that viscous force is negative
  force_total <- force_generated + force_viscous

  # Propulsive or Net force acting to accelerate the object
  force_propulsive <- force_total - weight

  # Get acceleration
  acceleration <- force_propulsive / mass

  # Save the force and acceleration to the data frame
  results <- data.frame(
    current_time = current_time,
    current_distance = current_distance,
    current_velocity = current_velocity,
    mass = mass,
    weight = weight,
    max_force = max_force,
    force_percentage = force_percentage,
    force_potential = force_potential,
    force_generated = force_generated,
    force_viscous = force_viscous,
    force_total = force_total,
    force_propulsive = force_propulsive,
    acceleration = acceleration
  )
  return(results)
}
