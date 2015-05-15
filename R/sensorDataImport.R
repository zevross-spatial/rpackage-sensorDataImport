
# *****************************************************************************
# Load data ---------------------------
# *****************************************************************************

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("A package to load and process environmental sensor data")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Your name goes here",
    devtools.desc.author = '"First Last <first.last@example.com> [aut, cre]"',
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}





#' @import dplyr



#' @importFrom magrittr %<>%
#' @name %<>%
#' @export
#' @usage xxx
NULL



