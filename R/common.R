
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


# string to string vector ----------------------------------------------

#' Transforms string into a vector in string format.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_string_vector <- function(str) {
  if (length(str) == 0) {
    res <- '""'
  } else if (str == "") {
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

# adapt_image ----------------------------------------------------

#' Fits an image to the given size.
#'
#' @param image_file A string, image file.
#' @param width A integer, width of the image.
#' @param height A integer, height of the image.
#'
#' @return A string, new image file.
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

# is numeric  ----------------------------------------------------

#' Check if it is numeric.
#'
#' @param str A string.
#'
#' @return A boolean.
#' @keywords internal
is_numeric <- function(str) {
  all(!is.na(suppressWarnings(as.numeric(str))))
}

# has gaps  ----------------------------------------------------

#' Check if it has gaps.
#'
#' @param str A string.
#'
#' @return A boolean.
#' @keywords internal
has_gaps <- function(str) {
  grepl('[[1]]', str, fixed = TRUE) & grepl('[[2]]', str, fixed = TRUE)
}
