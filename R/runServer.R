#'  Create a shiny-server GUI to interactively use the IOHanalyzer
#' @param port Optional; which port the server should be opened at. Defaults 
#' to the option set for 'shiny.port'
#' @param open_browser Whether or not to open a browser tab with the 
#' IOHanalyzer GUI. Defaults to TRUE.
#' @param orca_gpu Whether or not orca will be allowed to use gpu-accelleration for saving figures to file. 
#' @export
#' @examples 
#' \dontrun{
#' runServer(6563, TRUE)
#' }
runServer <- function(port = getOption('shiny.port'), open_browser = TRUE, orca_gpu = TRUE) {
  appDir <- system.file("shiny-server", package = "IOHanalyzer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IOHanalyzer`.", call. = FALSE)
  }
  options("IOHanalyzer.orca_use_gpu" = orca_gpu)

  shiny::runApp(appDir, port = port, launch.browser = open_browser, display.mode = "normal")
}
