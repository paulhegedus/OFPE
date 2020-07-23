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
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}


#' @title Put data in temp folder
#'
#' @description From OFPEDATA package to temp folder
#'
#' @param x file name in OFPEDATA package.
#' @param y file name to save data as in temp folder.
#' @return Result of function or code chunk.
#' @export
toTempFoldr <- function(x, y) {
  browser()

  x <- eval(parse(text = x))


  sf::st_write(x,
               paste0(temp_path, y, ".shp"),
               quiet = TRUE)
}



