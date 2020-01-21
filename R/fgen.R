#' Get Force Generator Force Percentage
#'
#' Due to the Force Generator Force-Length characteristic, maximal force is unable to be expressed across whole Push-off Distance.
#'    \code{fgen_get_force_percentage} is a function that represents the force percentage at \code{push_off_perc}
#' @param push_off_perc Numeric vector. Indicates percentage of the Push-off Distance. Values needs to be positive
#' @param start_perc Numeric vector. Indicates starting percentage. Allowed range from 0 to 1. Default is 0.8
#' @param threshold Numeric vector. Indicates threshold where the line starts to break. Use values from 0.8 to 1, where
#'     1 is equal to \code{push_off_perc} position. Default is 0.9
#' @return Numeric vector with values from 0 to 1, indicating Force Percentage at particular \code{push_off_perc}
#' @export
#' @examples
#' x <- seq(0, 1.1, length.out = 1000)
#'
#' y1 <- fgen_get_force_percentage(
#'   push_off_perc = x,
#'   start_perc = 0.6,
#'   threshold = 0.90
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_get_force_percentage(
#'   push_off_perc = x,
#'   start_perc = 0.8,
#'   threshold = 0.95
#' )
#'
#' lines(x, y2, col = "red")
fgen_get_force_percentage <- function(push_off_perc,
                                      start_perc = 0.8,
                                      threshold = 0.9) {
  if (any(push_off_perc < 0)) {
    stop("Push-off distance percentage (push_off_perc) cannot be below zero.", call. = FALSE)
  }

  if (any(start_perc < 0 | start_perc > 1)) {
    stop("Start percentage (start_perc) needs to be within 0.8 - 1.")
  }

  if (any(threshold > 1 | threshold < 0.8)) {
    stop("Threshold needs to be within 0.8 - 1.")
  }


  y1 <- (1 - start_perc) * sin((pi * push_off_perc) / (2 * threshold)) + start_perc
  alpha <- (-1) / (threshold - 1)^2
  y2 <- alpha * (push_off_perc - threshold)^2 + 1

  force_percentage <- ifelse(push_off_perc < threshold, y1, y2)
  force_percentage <- ifelse(push_off_perc > 1, 0, force_percentage)

  return(force_percentage)
}

#' Get Force Generator Activation Level
#'
#' Due to the Force Generator Force-Time characteristic, it takes time to activate the Force Generator.
#'     \code{fgen_get_activation} is a function that represents the activation level at \code{current_time}
#' @param current_time Numeric vector
#' @param initial_activation Numeric vector between 0 and 1. Default is 0
#' @param time_to_max_activation Numeric vector. Time it takes to reach maximal activation from 0. Default is 0.3s.
#' @return Numeric vector. Activation level at \code{current_time}
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 1000)
#'
#' y1 <- fgen_get_activation(
#'   current_time = x,
#'   initial_activation = 0,
#'   time_to_max_activation = 0.5
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_get_activation(
#'   current_time = x,
#'   initial_activation = 0.2,
#'   time_to_max_activation = 0.5
#' )
#'
#' lines(x, y2, col = "red")
#'
#' y3 <- fgen_get_activation(
#'   current_time = x,
#'   initial_activation = 0,
#'   time_to_max_activation = 0.4
#' )
#'
#' lines(x, y3, col = "blue")
fgen_get_activation <- function(current_time,
                                initial_activation = 0,
                                time_to_max_activation = 0.3) {
  if (any(current_time < 0)) {
    stop("Current time cannot be below zero.", call. = FALSE)
  }

  if (any(initial_activation < 0 | initial_activation > 1)) {
    stop("Initial activation must be between 0 and 1.", call. = FALSE)
  }

  if (any(time_to_max_activation < 0)) {
    stop("Time to maximal activation needs to be a positive number", call. = FALSE)
  }

  IES <- 1 / (time_to_max_activation)
  time_to_max_activation <- time_to_max_activation * (1 - initial_activation)

  alpha <- (4 * IES) / (1 - initial_activation) * 2.5

  activation <- (1 - initial_activation) / (1 + exp(-alpha * (current_time - time_to_max_activation / 2))) + initial_activation

  return(activation)
}


#' Get Force Generator Viscous Force
#'
#' Due to the Force Generator Force-Velocity characteristic, it velocity produces breaking or viscous force.
#'     \code{fgen_get_viscous_force} is a function that represents the viscous force at \code{current_velocity}
#' @param current_velocity Numeric vector
#' @param max_force Numeric value. Default is 3000 Newtons
#' @param max_velocity Numeric value. Default is 4 m/s
#' @param ... Used to allow different parameters to be passes without error
#' @return Viscous force at \code{current_velocity}
#' @export
#' @examples
#' x <- seq(0, 3, length.out = 1000)
#'
#' y1 <- fgen_get_viscous_force(
#'   current_velocity = x,
#'   max_velocity = 3,
#'   max_force = 1200
#' )
#'
#' plot(x, y1, "l")
#'
#' y2 <- fgen_get_viscous_force(
#'   current_velocity = x,
#'   max_velocity = 3,
#'   max_force = 1000
#' )
#'
#' lines(x, y2, col = "red")
fgen_get_viscous_force <- function(current_velocity,
                                   max_force = 3000,
                                   max_velocity = 4) {
  visc_factor <- max_force / max_velocity
  return(current_velocity * visc_factor)
}


#' Get Forve-Velocity Velocity of the Force Generator
#'
#' Based on the parameters of the Force Generator: \code{max_force} and \code{max_velocity}
#'     get maximal velocity that can be reached at particular \code{external_resistance}
#'
#' @param external_force Numeric vector. External force is in Newtons
#' @param max_force Numeric vector. Maximal force in Newton that Force Generator can generate
#' @param max_velocity Numeric vectpr. Maximal velocity that Force Generator can achieve in unconstrained conditions
#' @return Numeric vector of estimated maximal velocity reached
#' @export
#' @examples
#' fgen_get_velocity(0, 3000, 4)
#' fgen_get_velocity(3000, 3000, 4)
#' fgen_get_velocity(1500, 3000, 4)
fgen_get_velocity <- function(external_force, max_force = 3000, max_velocity = 4) {
  (max_force - external_force) / (max_force / max_velocity)
}


#' Get Instanteous Output Of The Force Generator
#'
#' \code{fgen_get_output} returns acceleration and force data based on the current system state (\code{current_time},
#'     \code{current_distance}, and \code{current_velocity}), system constraints (\code{mass}, \code{weight}, and \code{push_off_distance}),
#'     and parameters of the Forge Generator (\code{max_force}, \code{current_velocity}, \code{start_perc}, \code{threshold},
#'     and \code{time_to_max_activation})
#' #
#' @param current_time Numeric vector. Default is 0
#' @param current_distance Numeric vector. Default is 0
#' @param current_velocity Numeric vector. Default is 0
#' @param mass Numeric vector. Default is 75kg
#' @param weight Numeric vector. Default is mass x 9.81
#' @param push_off_distance Numeric vector. Default is 0.4
#' @param max_force Numeric value. Default is 3000N
#' @param max_velocity Numeric vector. Default is 4m/s
#' @param start_perc Numeric vector. Default is 0.8
#' @param threshold Numeric vector. Default is 0.9
#' @param time_to_max_activation Numeric vector. Default is 0.3s
#' @return List with five elements: kinematics, system_constraints, fgen_parameters, fgen_output, and kinetics
#' @export
#' @examples
#' state <- expand.grid(
#'   current_time = seq(0, 0.25, length.out = 1000),
#'   current_distance = c(0.1, 0.15, 0.2, 0.25, 0.3),
#'   current_velocity = 0
#' )
#'
#' fgen_data <- fgen_get_output(
#'   max_force = 3000,
#'   mass = 75,
#'   current_time = state$current_time,
#'   current_distance = state$current_distance,
#'   current_velocity = state$current_velocity
#' )
fgen_get_output <- function( # The parameters forwarded by `vj_simulate` function
                            # kinematics
                            current_time = 0,
                            current_distance = 0,
                            current_velocity = 0,

                            # system constrains
                            mass = 75,
                            weight = mass * 9.81,
                            push_off_distance = 0.4,

                            # These are the extra parameters `...`

                            max_force = 3000,
                            max_velocity = 4,

                            start_perc = 0.8,
                            threshold = 0.9,

                            time_to_max_activation = 0.3) {

  # Get percent of maximal force based on the current position (distance)
  push_off_perc <- current_distance / push_off_distance

  force_percentage <- fgen_get_force_percentage(
    push_off_perc = push_off_perc,
    start_perc = start_perc,
    threshold = threshold
  )

  # Maximal force that can be generate at particular position
  potential_force <- max_force * force_percentage

  # -----------------------------
  # Get Force Generator activation

  # To calculate activation we need initial force percentage (t=0, d=0)
  # NOT the current force percentage
  force_percentage_init <- fgen_get_force_percentage(
    push_off_perc = 0,
    start_perc = start_perc,
    threshold = threshold
  )
  potential_force_init <- max_force * force_percentage_init

  initial_activation <- weight / potential_force_init
  # ----------------------------

  activation <- fgen_get_activation(
    current_time = current_time,
    initial_activation = initial_activation,
    time_to_max_activation = time_to_max_activation
  )

  generated_force <- activation * potential_force

  # Viscous force
  viscous_force <- fgen_get_viscous_force(
    current_velocity = current_velocity,
    max_force = max_force,
    max_velocity = max_velocity
  )

  # ----
  # We need to "scale" viscous force as we did max force using force percentage
  viscous_force <- viscous_force * force_percentage

  # Total force, acting on the object
  ground_reaction_force <- generated_force - viscous_force

  # ----
  # check if GRF is below zero (which is imposible)
  if (any(ground_reaction_force < 0)) {
    ground_reaction_force <- ifelse(ground_reaction_force < 0, 0, ground_reaction_force)
    warning("GRF below zero. Please check your model", call. = FALSE)
  }

  # Propulsive or Net force acting to accelerate the object
  propulsive_force <- ground_reaction_force - weight

  # Get acceleration
  acceleration <- propulsive_force / mass

  # Save the force and acceleration to the object
  results <- list(
    # Kinematics data - current time, distance, velocity
    # These are optional to be returned in custom function
    kinematics = list(
      current_time = current_time,
      current_distance = current_distance,
      current_velocity = current_velocity
    ),

    # System constraints
    # These are optional to be returned in custom function
    system_constraints = list(
      mass = mass,
      weight = weight,
      push_off_distance = push_off_distance
    ),

    # Force Generator parameters
    # These are optional to be returned in custom function
    fgen_parameters = list(
      max_force = max_force,
      max_velocity = max_velocity,

      start_perc = start_perc,
      threshold = threshold,

      time_to_max_activation = time_to_max_activation,
      initial_activation = initial_activation
    ),

    # Force Generator  output and intermediary forces & variables
    # These are optional
    fgen_output = list(
      push_off_perc = push_off_perc,
      force_percentage = force_percentage,
      potential_force = potential_force,
      activation = activation,
      generated_force = generated_force,
      viscous_force = viscous_force
    ),

    # Resulting kinetics
    # These MUST be returned since they are used in `vj_simulate`
    kinetics = list(
      ground_reaction_force = ground_reaction_force,
      propulsive_force = propulsive_force,
      acceleration = acceleration
    )
  )
  return(results)
}
