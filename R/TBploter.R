#' Run the default TBploter app for analysis locally
#'
#' \code{TBploter} run TBploter locally
#' @author Qi Zhao

TBploter <- function() {
  shiny::runApp(system.file("TBploter", package = "TBploter"))
}
