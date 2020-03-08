#' Get Simple Take-Off Velocity
#'
#' \code{get_simple_take_off_velocity} return predicted take-off velocity at bodyweight load
#'     using Load~Take-Off Velocity profile
#' @param L0 Numeric vector
#' @param TOV0 Numeric vector
#' @param bodyweight Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' get_simple_take_off_velocity(L0 = 250, TOV0 = 3, bodyweight = 75)

#' @export
get_simple_take_off_velocity <- function(L0, TOV0, bodyweight) {
  TOV0 - (TOV0/L0 * bodyweight)
}


#' Get Simple Optimal Profile
#'
#' \code{get_simple_optimal_profile} calculates optimal profile using Load~TOV profile. This is done by
#'     assuming same 'surface' under the profile and getting the profile tha maximize TOV at bodyweight
#' @param L0 Numeric vector
#' @param TOV0 Numeric vector
#' @param bodyweight Numeric vector
#' @param gravity_const Numeric vector. Default is 9.81
#' @return List
#' @export
#' @examples
#' get_simple_optimal_profile(L0 = 250, TOV0 = 3, bodyweight = 75)
get_simple_optimal_profile <- function(L0, TOV0, bodyweight, gravity_const = 9.81) {

  surface <- (L0 * TOV0) / 2
  optimal_TOV0 <- surface / bodyweight
  optimal_L0 <- 2 * surface / optimal_TOV0

  take_off_velocity <- get_simple_take_off_velocity(
    L0 = L0,
    TOV0 = TOV0,
    bodyweight = bodyweight
    )
  Sfv <-  get_slope(L0, TOV0)

  optimal_take_off_velocity <- get_simple_take_off_velocity(
    L0 = optimal_L0,
    TOV0 = optimal_TOV0,
    bodyweight = bodyweight
  )
  optimal_Sfv <-  get_slope(optimal_L0, optimal_TOV0)


  # Probe
  take_off_velocity_f_increase <- get_simple_take_off_velocity(
    L0 = L0 * 1.1,
    TOV0 = TOV0,
    bodyweight = bodyweight
  )

  take_off_velocity_v_increase <- get_simple_take_off_velocity(
    L0 = L0,
    TOV0 = TOV0 * 1.1,
    bodyweight = bodyweight
  )

  # Results
  df <- list(
    L0 = L0,
    TOV0 = TOV0,
    Sfv = Sfv,
    take_off_velocity = take_off_velocity,
    height = get_height(take_off_velocity, gravity_const),

    optimal_L0 = optimal_L0,
    optimal_TOV0 = optimal_TOV0,
    optimal_Sfv = optimal_Sfv,
    optimal_take_off_velocity = optimal_take_off_velocity,
    optimal_height = get_height(optimal_take_off_velocity, gravity_const),

    Sfv_perc = (Sfv / optimal_Sfv) * 100,
    FV_imbalance = abs(1 - (Sfv / optimal_Sfv)) * 100,
    probe_IMB = (take_off_velocity_v_increase - take_off_velocity) / (take_off_velocity_f_increase - take_off_velocity) * 100
  )

  return(df)
}


#' Get Simple Profile
#'
#' \code{get_simple_profile} take the \code{rofile_data} and performs Load~TOV profile and returns
#'     the optimal profile using \code{\link{get_simple_optimal_profile}} function
#' @param profile_data Data frame returned from \code{\link{vj_profile}} function
#' @return List returned by \code{\link{get_simple_optimal_profile}} function
#' @export
#' @examples
#' fv_profile <- vj_profile(mass = 75)
#' get_simple_profile(fv_profile)
get_simple_profile <- function(profile_data) {

  # Get Load~TOV profile
  jump_profile <- get_FV_profile(
    profile_data,
    force = "mass",
    velocity = "take_off_velocity"
  )

  get_simple_optimal_profile(
    L0 = jump_profile$F0,
    TOV0 = jump_profile$V0,
    bodyweight = profile_data$bodyweight[1],
    gravity_const = profile_data$gravity_const[1]
    )
}


#' Probe Simple Take-off Velocity
#'
#' \code{probe_simple_take_off_velocity} probes results of the \code{\link{get_simple_take_off_velocity}} function by varying
#' \code{L0}, \code{TOV0}, and \code{bodyweight} parameters
#' @param L0 Numeric vector
#' @param TOV0 Numeric vector
#' @param bodyweight Numeric vector
#' @param change_ratio Numeric vector indicating probing change ratios
#' @param aggregate How should \code{\link{probe_simple_take_off_velocity}} output be aggregated?
#'     Default is "raw". Other options involve "ratio" and "diff" which use initial
#'     output values
#' @return Probing data frame
#' @export
#' @examples
#' require(ggplot2)
#'
#' simple_probe_data <- probe_simple_take_off_velocity(
#'   L0 = 250,
#'   TOV0 = 3,
#'   bodyweight = 75,
#'   change_ratio = seq(0.8, 1.2, length.out = 1001)
#' )
#'
#' ggplot(
#'   simple_probe_data,
#'   aes(
#'     x = change_ratio,
#'     y = take_off_velocity,
#'     color = probing
#'   )
#' ) +
#' geom_line()
probe_simple_take_off_velocity <- function(L0,
                                           TOV0,
                                           bodyweight,
                                           change_ratio = seq(0.9, 1.1, length.out = 3),
                                           aggregate = "raw"
) {
  get_probing_data(
    args_list = list(L0 = L0, TOV0 = TOV0, bodyweight = bodyweight),
    probe_func = function(...) list(take_off_velocity = get_simple_take_off_velocity(...)),
    change_ratio = change_ratio,
    aggregate = aggregate
  )
}

