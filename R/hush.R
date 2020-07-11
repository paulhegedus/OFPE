#' @title Truly invisible output
#'
#' @description Suppress all output from function return. Useful for st_write. Credit goes
#' to "Danny", https://stackoverflow.com/users/699385/danny.
#'
#' @param code Function or code.
#' @return Execution of function or code.
#' @source \url{https://stackoverflow.com/questions/2723034/suppress-output-of-a-function}
#' @export
hush <- function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}
