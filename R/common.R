# vector to string ----------------------------------------------------

#' Transforms a vector of strings into a string.
#'
#' @param vector A vector of strings.
#'
#' @return A string.
#' @keywords internal
vector_to_string <- function(vector) {
  if (is.null(vector)) {
    res <- ""
  } else {
    res <- paste(vector, collapse = "<|>")
  }
  res
}

# string to vector ----------------------------------------------------

#' Transforms string into a vector of strings.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_vector <- function(str) {
  if (str == "") {
    res <- NULL
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
  }
  res
}
