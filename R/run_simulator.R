#' Runs the Vertical Jump Shiny Simulator
#' @export
run_simulator <- function() {
  appDir <- system.file("shiny-simulator", package = "vjsim")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vjsim`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
