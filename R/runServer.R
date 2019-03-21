#'  Create a shiny-server GUI to interactively use the IOHProfiler
#'
#'
#' @export
runServer <- function(){
  appDir <- system.file("shiny-server", package = "IOHProfiler")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IOHProfiler`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
