
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


#' Create a question data frame
#'
#' Creates an empty question data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' df <- create_question_data_frame()
#'
#' @return A data frame.
#' @export
create_question_data_frame <- function() {
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


#' Create a question csv file
#'
#' Creates an empty question csv file.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- create_question_csv(file = tempfile(fileext = '.csv'))
#'
#' @return A string.
#' @export
create_question_csv <- function(file, sep = ',') {
  questions <- create_question_data_frame()
  if (sep == ',') {
    utils::write.csv(questions, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(questions, file = file, row.names = FALSE)
  }
  invisible(file)
}


#' Read a question csv file
#'
#' Reads a csv file of questions and returns a data frame.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- system.file("extdata", "questions.csv", package = "moodef")
#' df <- read_question_csv(file = file)
#'
#' @return A data frame.
#' @export
read_question_csv <- function(file, sep = ',') {
  df <- readr::read_delim(
    file,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character())
  )
  attributes <- names(df)
  df[, attributes] <- data.frame(lapply(df[, attributes], as.character), stringsAsFactors = FALSE)
  df[, attributes] <-
    apply(df[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ''))
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  df
}
