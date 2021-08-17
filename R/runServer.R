#'  Create a shiny-server GUI to interactively use the IOHanalyzer
#' @param port Optional; which port the server should be opened at. Defaults 
#' to the option set for 'shiny.port'
#' @param open_browser Whether or not to open a browser tab with the 
#' IOHanalyzer GUI. Defaults to TRUE.
#' @export
#' @examples 
#' \dontrun{
#' runServer(6563, TRUE)
#' }
runServer <- function(port = getOption('shiny.port'), open_browser = TRUE) {
  appDir <- system.file("shiny-server", package = "IOHanalyzer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IOHanalyzer`.", call. = FALSE)
  }

  shiny::runApp(appDir, port = port, launch.browser = open_browser, display.mode = "normal")
}
