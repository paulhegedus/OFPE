#' @title Truly invisible output
#'
#' @description Suppress all output from function return. Useful
#' for st_write and other functions that have chatty outputs. Credit
#' goes to "Danny", https://stackoverflow.com/users/699385/danny.
#'
#' @param code Function or code chunk.
#' @return Result of function or code chunk.
#' @source \url{https://stackoverflow.com/questions/2723034/suppress-output-of-a-function}
#' @export
hush <- function(code){
  sink("~/NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}
