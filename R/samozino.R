#' Get Maximal Take-off Velocty
#'
#' \code{get_samozino_take_off_velocity} returns maximal take off velocity that could be achieve based on the Samozino \emph{et al.}
#'     model which uses vertical jump profile \code{F0} and \code{V0}.
#' @param F0 Numeric vector. Default 3000
#' @param V0 Numeric vector. Default 4
#' @param bodyweight Numeric vector. Default 75
#' @param push_off_distance Numeric vector. Default 0.4
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector representing maximal take-off velocity
#' @export
#' @references
#'     Samozino, Pierre. ‘A Simple Method for Measuring Lower Limb Force, Velocity and Power Capabilities During Jumping’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 65–96. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_4.
#'
#'     ———. ‘Optimal Force-Velocity Profile in Ballistic Push-off: Measurement and Relationship with Performance’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 97–119. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_5.
#'
#'     Samozino, Pierre, Jean-Benoît Morin, Frédérique Hintzy, and Alain Belli. ‘Jumping Ability: A Theoretical Integrative Approach’. Journal of Theoretical Biology 264, no. 1 (May 2010): 11–18. https://doi.org/10.1016/j.jtbi.2010.01.021.
#'
#'     Samozino, Pierre, Enrico Rejc, Pietro Enrico Di Prampero, Alain Belli, and Jean-Benoît Morin. ‘Optimal Force–Velocity Profile in Ballistic Movements—Altius’: Medicine & Science in Sports & Exercise 44, no. 2 (February 2012): 313–22. https://doi.org/10.1249/MSS.0b013e31822d757a.
#' @examples
#' get_samozino_take_off_velocity(F0 = 2500, V0 = 3.7, bodyweight = 85, push_off_distance = 0.42)
get_samozino_take_off_velocity <- function(F0 = 3000,
                                           V0 = 4,
                                           bodyweight = 75,
                                           push_off_distance = 0.4,
                                           gravity_const = 9.81) {
  (push_off_distance * (sqrt(F0^2 / (4 * V0^2) - (2 * bodyweight * (gravity_const * bodyweight - F0)) / push_off_distance) - F0 / (2 * V0))) / bodyweight
}

#' Get Samozino's Optimal Profile
#'
#'  \code{get_samozino_optimal_profile} finds optimal Force-Velocity profile that maximizes vertical jump height based on the model of
#'  Samozino \emph{et al.}. According to this model, we are looking for Force-Velocity profile that maintains the max power, but changes
#'  \code{F0} and \code{V0} so it maximized the take-off velocity calculated using \code{\link{get_samozino_take_off_velocity}}.
#' @param F0 Numeric value. Default 3000
#' @param V0 Numeric value. Default 4
#' @param bodyweight Numeric value. Default 75
#' @param push_off_distance Numeric value. Default 0.4
#' @param gravity_const Numeric value. Default 9.81
#' @return List with list and data frame
#' @export
#' @references
#'     Samozino, Pierre. ‘A Simple Method for Measuring Lower Limb Force, Velocity and Power Capabilities During Jumping’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 65–96. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_4.
#'
#'     ———. ‘Optimal Force-Velocity Profile in Ballistic Push-off: Measurement and Relationship with Performance’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 97–119. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_5.
#'
#'     Samozino, Pierre, Jean-Benoît Morin, Frédérique Hintzy, and Alain Belli. ‘Jumping Ability: A Theoretical Integrative Approach’. Journal of Theoretical Biology 264, no. 1 (May 2010): 11–18. https://doi.org/10.1016/j.jtbi.2010.01.021.
#'
#'     Samozino, Pierre, Enrico Rejc, Pietro Enrico Di Prampero, Alain Belli, and Jean-Benoît Morin. ‘Optimal Force–Velocity Profile in Ballistic Movements—Altius’: Medicine & Science in Sports & Exercise 44, no. 2 (February 2012): 313–22. https://doi.org/10.1249/MSS.0b013e31822d757a.
#' @examples
#' get_samozino_optimal_profile(F0 = 2500, V0 = 3.7, bodyweight = 85, push_off_distance = 0.42)
get_samozino_optimal_profile <- function(F0 = 3000,
                                         V0 = 4,
                                         bodyweight = 75,
                                         push_off_distance = 0.4,
                                         gravity_const = 9.81) {

  # Dataframe to be forwarded to optim function
  data <- data.frame(
    F0 = F0,
    V0 = V0,
    bodyweight = bodyweight,
    push_off_distance = push_off_distance,
    gravity_constant = gravity_const
  )

  # function to be returned to optim
  take_off_velocity <- get_samozino_take_off_velocity(F0, V0, bodyweight, push_off_distance, gravity_const)

  opt_jump_func <- function(par, data) {
    new_F0 <- data$F0 / par[1]
    new_V0 <- data$V0 * par[1]

    take_off_velocity <- get_samozino_take_off_velocity(
      new_F0,
      new_V0,
      data$bodyweight,
      data$push_off_distance,
      data$gravity_const
    )
    return(1 / take_off_velocity)
  }
  # ------------------------
  # Find optimal parameters
  upper_bound <- (F0 / bodyweight) / gravity_const

  results <- stats::optim(par = 1, fn = opt_jump_func, data = data, method = "Brent", lower = 0, upper = upper_bound)

  # Save the results
  Sfv <- get_slope(F0, V0)
  optimal_F0 <- F0 / results$par[1]
  optimal_V0 <- V0 * results$par[1]
  optimal_Sfv <- get_slope(optimal_F0, optimal_V0)
  optimal_take_off_velocity <- 1 / results$value
  optimal_height <- get_height(optimal_take_off_velocity, gravity_const)
  optimal_Pmax <- get_max_power(optimal_F0, optimal_V0)

  df <- list(
    F0 = F0,
    F0_rel = F0 / bodyweight,
    V0 = V0,
    Pmax = get_max_power(F0, V0),
    Pmax_rel = get_max_power(F0, V0) / bodyweight,
    Sfv = Sfv,
    take_off_velocity = take_off_velocity,
    height = get_height(take_off_velocity, gravity_const),
    optimal_F0 = optimal_F0,
    optimal_F0_rel = optimal_F0 / bodyweight,
    optimal_V0 = optimal_V0,
    optimal_height = optimal_height,
    optimal_Pmax = optimal_Pmax,
    optimal_Pmax_rel = optimal_Pmax / bodyweight,
    optimal_take_off_velocity = optimal_take_off_velocity,
    optimal_Sfv = optimal_Sfv,
    Sfv_perc = (Sfv / optimal_Sfv),
    FV_imbalance = abs(1 - (Sfv / optimal_Sfv))
  )
  return(df)
}

#' Get All Samozino profiles
#'
#' \code{get_all_samozino_profiles} returns theoretical and practical Samozino profiles using \code{profile_data}
#' returned from the \code{\link{vj_profile}} and \code{\link{get_samozino_optimal_profile}} functions
#' @param profile_data Data frame returned from \code{\link{vj_profile}} function
#' @return List with two elements: list of profiles and long data frame
#' @export
#' @references
#'     Samozino, Pierre. ‘A Simple Method for Measuring Lower Limb Force, Velocity and Power Capabilities During Jumping’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 65–96. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_4.
#'
#'     ———. ‘Optimal Force-Velocity Profile in Ballistic Push-off: Measurement and Relationship with Performance’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 97–119. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_5.
#'
#'     Samozino, Pierre, Jean-Benoît Morin, Frédérique Hintzy, and Alain Belli. ‘Jumping Ability: A Theoretical Integrative Approach’. Journal of Theoretical Biology 264, no. 1 (May 2010): 11–18. https://doi.org/10.1016/j.jtbi.2010.01.021.
#'
#'     Samozino, Pierre, Enrico Rejc, Pietro Enrico Di Prampero, Alain Belli, and Jean-Benoît Morin. ‘Optimal Force–Velocity Profile in Ballistic Movements—Altius’: Medicine & Science in Sports & Exercise 44, no. 2 (February 2012): 313–22. https://doi.org/10.1249/MSS.0b013e31822d757a.
#' @examples
#' fv_profile <- vj_profile(mass = 75)
#' all_samozino_profiles <- get_all_samozino_profiles(fv_profile)
#' all_samozino_profiles$data_frame
get_all_samozino_profiles <- function(profile_data) {

  # Theoretical Samozino profile
  # Uses mean velocity of the profile
  jump_profile <- get_FV_profile(
    profile_data,
    force = "mean_GRF_over_distance",
    velocity = "mean_velocity"
  )

  samozino_theoretical_profile <- get_samozino_optimal_profile(
    F0 = jump_profile$F0,
    V0 = jump_profile$V0,
    bodyweight = profile_data$bodyweight[1],
    push_off_distance = profile_data$push_off_distance[1],
    gravity_const = profile_data$gravity_const[1]
  )

  # Practical Samozino profile
  # Uses half of take-off velocity as mean velocity
  # This is easier to measure without laboratory conditions
  jump_profile <- get_FV_profile(
    profile_data,
    force = "mean_GRF_over_distance",
    velocity = "mean_velocity_as_TOV_half"
  )

  samozino_practical_profile <- get_samozino_optimal_profile(
    F0 = jump_profile$F0,
    V0 = jump_profile$V0,
    bodyweight = profile_data$bodyweight[1],
    push_off_distance = profile_data$push_off_distance[1],
    gravity_const = profile_data$gravity_const[1]
  )

  # Bind them together
  profiles_list <- list(
    samozino_theoretical_profile = samozino_theoretical_profile,
    samozino_practical_profile = samozino_practical_profile
  )

  profiles_df <- dplyr::bind_rows(profiles_list, .id = "profile")
  profiles_df <- tidyr::gather(profiles_df, "metric", "value", -1, na.rm = TRUE)
  profiles_df$profile <- factor(
    profiles_df$profile,
    levels = c(
      "samozino_practical_profile",
      "samozino_theoretical_profile"
    ),
    labels = c(
      "Samozino practical profile",
      "Samozino theoretical profile"
    )
  )

  # Sort data frame
  profiles_df <- profiles_df[order(profiles_df$profile), ]

  return(list(
    list = profiles_list,
    data_frame = profiles_df
  ))
}


#' Get Samozino Jump Metrics
#'
#' \code{get_samozino_jump_parameters} return mean force, mean velocity and mean power using simple method explained in
#' the references.
#' @param mass Numeric vector. Default 75
#' @param push_off_distance Numeric vector. Default 0.4
#' @param height Numeric vector. Jump height. Default 0.5
#' @param gravity_const Numeric vector. Default 9.81
#' @return List with \code{mean_GRF_over_distance}, \code{mean_velocity}, and \code{mean_power}
#' @references
#'     Samozino, Pierre. ‘A Simple Method for Measuring Lower Limb Force, Velocity and Power Capabilities During Jumping’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 65–96. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_4.
#'
#'     ———. ‘Optimal Force-Velocity Profile in Ballistic Push-off: Measurement and Relationship with Performance’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 97–119. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_5.
#'
#'     Samozino, Pierre, Jean-Benoît Morin, Frédérique Hintzy, and Alain Belli. ‘Jumping Ability: A Theoretical Integrative Approach’. Journal of Theoretical Biology 264, no. 1 (May 2010): 11–18. https://doi.org/10.1016/j.jtbi.2010.01.021.
#'
#'     Samozino, Pierre, Enrico Rejc, Pietro Enrico Di Prampero, Alain Belli, and Jean-Benoît Morin. ‘Optimal Force–Velocity Profile in Ballistic Movements—Altius’: Medicine & Science in Sports & Exercise 44, no. 2 (February 2012): 313–22. https://doi.org/10.1249/MSS.0b013e31822d757a.
#' @export
#' @examples
#' get_samozino_jump_metrics(
#'   mass = 75,
#'   push_off_distance = 0.43,
#'   height = 0.45
#' )
get_samozino_jump_metrics <- function(mass = 75,
                                      push_off_distance = 0.4,
                                      height = 0.5,
                                      gravity_const = 9.81) {
  mean_GRF_over_distance <- mass * gravity_const * (height / push_off_distance + 1)
  mean_velocity <- sqrt(gravity_const * height / 2)
  mean_power <- mean_GRF_over_distance * mean_velocity

  return(list(
    mean_GRF_over_distance = mean_GRF_over_distance,
    mean_velocity = mean_velocity,
    mean_power = mean_power
  ))
}

