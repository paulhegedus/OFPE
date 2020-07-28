#' @title Remove special characters from strings
#'
#' @description Removes any special characters from a string. Includes all
#' single special characters as well as ".-" and "__".
#'
#' @param STRNG Character string for cleaning.
#' @param FILES Logical, are the strings filenames? If TRUE, "." not removed,
#' if FALSE, "." removed.
#' @return Clean character string.
#' @export
noSpecialChar <- function(STRNG, FILES) { # removes special characters from filenames
  STRNG <- gsub(".-", "", STRNG, fixed = TRUE)
  STRNG <- gsub("%| |,|~|!|@|#|$|^|&|*|+|=|/|?|>|<|;|:|\\(|\\)|\\{|\\[|\\]",
                "",
                STRNG)
  STRNG <- gsub("__", "_", STRNG, fixed=TRUE)
  if (!FILES) {
    STRNG <- gsub(".", "_", STRNG, fixed = TRUE)
  }
  return(STRNG)
}
