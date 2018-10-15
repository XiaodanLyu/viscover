#' Run the overlay tool
#'
#' @examples
#' ## not run
#' # runTool()
#' ## print the directory containing the code for the application
#' system.file("shiny-examples", "overlay", package = "viscover")
#' @export
runTool <- function() {
  appDir <- system.file("shiny-examples", "overlay", package = "viscover")
  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}
