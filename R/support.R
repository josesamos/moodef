
#' Transforms a vector of strings into a string
#'
#' Insert the separator that we consider to later perform the reverse operation.
#'
#' @param vector A vector of strings.
#'
#' @family support functions
#'
#' @examples
#'
#' s <- vector_to_string(c('Addition', '+'))
#'
#' @return A string.
#' @export
vector_to_string <- function(vector) {
  if (is.null(vector)) {
    res <- ""
  } else {
    res <- paste(vector, collapse = "<|>")
  }
  res
}


#' Define a question data frame
#'
#' Defines an empty question data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' df <- define_question_data_frame()
#'
#' @return A data frame.
#' @export
define_question_data_frame <- function() {
  questions <-  data.frame(
    type = character(),
    question = character(),
    image = character(),
    image_alt = character(),
    answer = character(),
    a_1 = character(),
    a_2 = character(),
    a_3 = character(),
    stringsAsFactors = FALSE
  )
  questions
}


#' Define a question csv file
#'
#' Defines an empty question csv file.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- define_question_csv(file = tempfile(fileext = '.csv'))
#'
#' @return A string.
#' @export
define_question_csv <- function(file, sep = ',') {
  questions <- define_question_data_frame()
  if (sep == ',') {
    utils::write.csv(questions, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(questions, file = file, row.names = FALSE)
  }
  invisible(file)
}
