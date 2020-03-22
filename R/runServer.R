#'  Create a shiny-server GUI to interactively use the IOHanalyzer
#' @param port Optional; which port the server should be opened at
#' @param browser Optional; if TRUE, open the set up URL in the default browser
#' @export
#' @examples 
#' \donttest{
#' runServer()
#' }
runServer <- function( port = getOption('shiny.port'), browser = FALSE ) {
  appDir <- system.file("shiny-server", package = "IOHanalyzer")
  if (appDir == "") {
    stop("Could not find example directory. Try to re-install `IOHanalyzer`.", call. = FALSE)
  }

  shiny::runApp(appDir, port = port, launch.browser = browser, display.mode = "normal")
}
