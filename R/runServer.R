#'  Create a shiny-server GUI to interactively use the IOHanalyzer
#' @param port Optional; which port the server should be opened at
#' @export
#' @examples 
#' \donttest{
#' runServer()
#' }
runServer <- function(port = getOption('shiny.port')) {
  appDir <- system.file("shiny-server", package = "IOHanalyzer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IOHanalyzer`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", port = port)
}
