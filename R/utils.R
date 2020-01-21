#' Get Height
#'
#' Function calculates height reached using the ballistic equation
#' @param take_off_velocity Numeric vector
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' get_height(2.75)
get_height <- function(take_off_velocity, gravity_const = 9.81) {
  (take_off_velocity^2) / (2 * gravity_const)
}


#' Get Power
#'
#' Function calculates maximal power assuming linear relationship between
#'     \code{max_force} and \code{max_velocity}
#' @param max_force Numeric vector
#' @param max_velocity Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' get_power(3000, 4)
get_power <- function(max_force, max_velocity) {
  (max_force * max_velocity) / 4
}
