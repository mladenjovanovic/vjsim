#' Get Height
#'
#' Function \code{get_height} calculates height reached using the ballistic equation
#' @param take_off_velocity Numeric vector
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' get_height(2.75)
get_height <- function(take_off_velocity, gravity_const = 9.81) {
  (take_off_velocity^2) / (2 * gravity_const)
}


#' Get Max Power
#'
#' Function \code{get_max_power} calculates maximal power assuming linear relationship between
#'     \code{max_force} and \code{max_velocity}
#' @param max_force Numeric vector
#' @param max_velocity Numeric vector.
#' @return Numeric vector
#' @export
#' @examples
#' get_max_power(3000, 4)
get_max_power <- function(max_force, max_velocity) {
  (max_force * max_velocity) / 4
}

#' Get Mean Power
#'
#' Function \code{get_mean_power} calculates the vertical jump mean power using kinetic and potential enery
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param push_off_distance Numeric vector
#' @param time_taken Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_mean_power(100, 100*9.81, 4, 0.4, 0.3)
get_mean_power <- function(mass,
                           weight = mass * 9.81,
                           take_off_velocity,
                           push_off_distance,
                           time_taken) {
  kinetic_energy <- (mass * take_off_velocity^2) / 2
  potential_energy <- weight * push_off_distance

  total_energy <- kinetic_energy + potential_energy

  # mean power
  total_energy / time_taken
}

#' Get Mean Force over Distance
#'
#' Function \code{get_mean_force_over_distance} calculates mean force over push-off distance using work equation
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param push_off_distance Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_mean_force_over_distance(100, 100*9.81, 4, 0.4)
get_mean_force_over_distance <- function(mass,
                                         weight = mass * 9.81,
                                         take_off_velocity,
                                         push_off_distance) {
  weight + (mass * (take_off_velocity^2) / (2*push_off_distance))
}


#' Get Mean Force over Time
#'
#' Function \code{get_mean_force_over_time} calculates mean force over push-off time using impulse equation
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param time_taken Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_mean_force_over_time(100, 100*9.81, 4, 0.3)
get_mean_force_over_time <- function(mass,
                                         weight = mass * 9.81,
                                         take_off_velocity,
                                         time_taken) {
  (mass * take_off_velocity / (time_taken)) + (weight)
}
