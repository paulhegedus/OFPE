#' @title Remove special characters from strings
#'
#' @description Removes any special characters from a string. Includes all
#' single special characters as well as ".-" and "__".
#'
#' @param strng Character string for cleaning.
#' @param files Logical, are strings filenames, if TRUE, "." not removed,
#' if FALSE, "." removed.
#' @return Clean character string.
#' @export
noSpecialChar <- function(strng, files) { # removes special characters from filenames
  strng <- gsub(".-", "", strng, fixed = TRUE)
  strng <- gsub("%| |,|~|!|@|#|$|^|&|*|+|=|/|?|>|<|;|:|\\(|\\)|\\{|\\[|\\]",
                "",
                strng)
  strng <- gsub("__", "_", strng, fixed=TRUE)
  if (!files) {
    strng <- gsub(".", "_", strng, fixed = TRUE)
  }
  return(strng)
}
