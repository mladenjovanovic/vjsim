#' Example testing data
#'
#' Profiling data for N=5 athletes using progressive squat jump measured using jump mat.
#'     Jump mat has a measurement error, with systematic bias of 0, proportional bias of 1
#'     and random error normally distributed with SD of 0.01s
#'
#' \describe{
#'    \item{athlete}{Character string}
#'    \item{bodyweight}{Bodyweight in kilograms}
#'    \item{push_off_distance}{Push-off distance during squat jump in meters}
#'    \item{external_load}{External load added to bodyweight in kilograms}
#'    \item{aerial_time}{Aerial time measured with jump mat in seconds}
#' }
#' @usage data(testing_data)
"testing_data"
