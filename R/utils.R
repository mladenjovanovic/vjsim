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

#' Get Force Velocity Profile Slop
#'
#' Function \code{get_slope} calculates force-velocity profile slope assuming linear relationship
#'    between \code{max_force} and \code{max_velocity}
#' @param max_force Numeric vector
#' @param max_velocity Numeric vector.
#' @return Numeric vector
#' @export
#' @examples
#' get_slope(3000, 4)
get_slope <- function(max_force, max_velocity) {
  -max_force/max_velocity
}

#' Get Work Done
#'
#' Function \code{get_work} calculates the work needed for the vertical jump using
#'     kinetic and potential energy
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param push_off_distance Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_work(100, 100 * 9.81, 4, 0.4)
get_work <- function(mass,
                     weight = mass * 9.81,
                     take_off_velocity,
                     push_off_distance) {
  kinetic_energy <- (mass * take_off_velocity^2) / 2
  potential_energy <- weight * push_off_distance

  total_energy <- kinetic_energy + potential_energy

  return(total_energy)
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
#' get_mean_power(100, 100 * 9.81, 4, 0.4, 0.3)
get_mean_power <- function(mass,
                           weight = mass * 9.81,
                           take_off_velocity,
                           push_off_distance,
                           time_taken) {
  total_energy <- get_work(
    mass = mass,
    weight = weight,
    take_off_velocity = take_off_velocity,
    push_off_distance = push_off_distance
  )

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
#' get_mean_force_over_distance(100, 100 * 9.81, 4, 0.4)
get_mean_force_over_distance <- function(mass,
                                         weight = mass * 9.81,
                                         take_off_velocity,
                                         push_off_distance) {
  weight + (mass * (take_off_velocity^2) / (2 * push_off_distance))
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
#' get_mean_force_over_time(100, 100 * 9.81, 4, 0.3)
get_mean_force_over_time <- function(mass,
                                     weight = mass * 9.81,
                                     take_off_velocity,
                                     time_taken) {
  (mass * take_off_velocity / (time_taken)) + (weight)
}


#' Get Impulse
#'
#' @param mass Numeric vector
#' @param take_off_velocity Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_impulse(75, 2)
get_impulse <- function(mass,
                       take_off_velocity) {
  mass * take_off_velocity
}

#' Get Take-off Velocity
#'
#' \code{get_take_off_velocity} returns take off velocity when \code{mean_force} is applied to the object
#' of \code{mass} over \code{push_off_distance} in vertical direction, assuming zero starting velocity
#' @param mean_force Numeric vector. Default 3000
#' @param mass Numeric vector. Default 75
#' @param push_off_distance Numeric vector. Default 0.4
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' get_take_off_velocity(
#'   mean_force = 2000,
#'   mass= 85,
#'   push_off_distance = 0.4
#' )
get_take_off_velocity <- function(mean_force = 3000,
                                  mass = 75,
                                  push_off_distance = 0.4,
                                  gravity_const = 9.81) {


  ifelse(
    mean_force < mass * gravity_const,
    NA,
    sqrt(2*push_off_distance*(mean_force / mass - gravity_const))
    )

}
