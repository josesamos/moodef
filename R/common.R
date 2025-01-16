#' Transform a String into a Vector of Strings
#'
#' Splits a string into a vector of strings using "<|>" as a delimiter.
#'
#' @param str `character`
#'   A string to transform.
#'
#' @return A vector of strings (`character`), or `NULL` if the input string is empty.
#' @keywords internal
string_to_vector <- function(str) {
  if (str == "") {
    res <- NULL
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
  }
  res
}

#' Transform a String into a String-Formatted Vector
#'
#' Converts a string into a string-formatted vector (e.g., `"a<|>b<|>c"` -> `c("a", "b", "c")`).
#'
#' @param str `character`
#'   A string to transform.
#'
#' @return A string representing a vector in R syntax (e.g., `c("a", "b", "c")`), or `"\"\""` if the input string is empty.
#' @keywords internal
string_to_string_vector <- function(str) {
  if (length(str) == 0 || str == "") {
    res <- '""'
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
    if (length(res) > 1) {
      res <- paste(res, collapse = '", "')
      res <- paste0('c("', res, '")')
    } else {
      res <- paste0('"', res, '"')
    }
  }
  res
}

#' Resize and Center an Image
#'
#' Resizes an image to fit within the specified dimensions, centering it on a blank canvas.
#'
#' @param image_file `character`
#'   Path to the image file to be resized.
#' @param width `integer`, default `800`
#'   Desired width of the output image.
#' @param height `integer`, default `600`
#'   Desired height of the output image.
#'
#' @return A string representing the path to the new resized image file.
#' @keywords internal
adapt_image <- function(image_file, width = 800, height = 600) {
  fig <- magick::image_blank(width = width, height = height, color = "white")
  img <- magick::image_read(image_file)
  img <- magick::image_trim(img)
  img <- magick::image_scale(img, sprintf("%dx%d", width - 50, height - 50))
  img <- magick::image_composite(fig, img, gravity = "Center")
  file <- tempfile(pattern = tools::file_path_sans_ext(basename(image_file)), fileext = ".png")
  magick::image_write(img, path = file, format = "png")
  file
}

#' Check if a String Represents a Numeric Value
#'
#' Verifies if a string can be converted to a numeric value.
#'
#' @param str `character`
#'   A string to check.
#'
#' @return `logical`
#'   `TRUE` if the string is numeric, `FALSE` otherwise.
#' @keywords internal
is_numeric <- function(str) {
  all(!is.na(suppressWarnings(as.numeric(str))))
}

#' Check if a String Contains Gaps
#'
#' Checks if a string contains the specific substrings `"[[1]]"` and `"[[2]]"`.
#'
#' @param str `character`
#'   A string to check.
#'
#' @return `logical`
#'   `TRUE` if the string contains gaps, `FALSE` otherwise.
#' @keywords internal
has_gaps <- function(str) {
  grepl("[[1]]", str, fixed = TRUE) & grepl("[[2]]", str, fixed = TRUE)
}
