#' Run Shiny App
#'
#' This function launches a Shiny application from the specified directory in an R package.
#' 
#' @param app_name bibfix
#' @return Launches the Shiny application.
#' @export
runBibfixApp <- function(app_name="bibfix") {
  # Ensure the Shiny package is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required but not installed.")
  }
  
  # Define the app directory
  app_dir <- here::here("inst/shiny-examples/bibfix")
  
  # Check if the app directory exists
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("App directory not found. Ensure the app is located in inst/shiny/", app_name)
  }
  
  # Launch the app
  shiny::runApp(app_dir, display.mode = "normal")
}
