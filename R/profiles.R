#' Vertical Jump Profile
#'
#' \code{vj_profile} generates vertical jump profile by loading the vertical jump with \code{external_load}
#' @param external_load Numeric vector. Default is  \code{c(-40, -20, 0, 20, 40, 60, 80)}
#' @param mass Numeric value. Represents bodyweight
#' @param ... Parameters forwarded to \code{\link{vj_simulate}}
#' @return Data frame. Returns the data frame from \code{\link{vj_simulate}} with extra two columns:
#'     \code{bodyweigh}t and \code{external_load}
#' @export
#' @examples
#' vj_profile_data <- vj_profile(mass = 75)
#'
#' plot(x = vj_profile_data$mass, y = vj_profile_data$height)
#' plot(x = vj_profile_data$mean_GRF_over_distance, y = vj_profile_data$mean_velocity)
#'
vj_profile <- function(external_load = c(-40, -20, 0, 20, 40, 60),
                       mass = 75,
                       ...) {
  profile <- data.frame()
  bw <- mass

  for (i in seq_along(external_load)) {
    new_mass <- external_load[i] + mass
    vj_summary <- vj_simulate(mass = new_mass, save_trace = FALSE, ...)
    vj_summary <- vj_summary$summary

    profile <- rbind(
      profile,
      data.frame(
        bodyweight = bw,
        external_load = external_load[i],
        vj_summary
      )
    )
  }
  return(profile)
}
