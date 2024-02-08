

#' Define questions from a csv file
#'
#' Each row in the text file is interpreted as a question. We only have to define
#' the columns that we are going to use, the rest of the columns are taken by default.
#'
#' For answers where a vector is required, "<|>" is used as a separator of the vector
#' elements.
#'
#' @param qc A `question_category` object.
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test')
#'
#' @export
define_questions_from_csv <- function(qc, file, sep)
  UseMethod("define_questions_from_csv")

#' @rdname define_questions_from_csv
#' @export
define_questions_from_csv.question_category <- function(qc, file, sep = ',') {
  table <- readr::read_delim(
    file,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character())
  )
  attributes <- names(table)
  table[, attributes] <-
    apply(table[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ''))
  attributes <- snakecase::to_snake_case(attributes)
  names(table) <- attributes
  for (opcional in c('type', 'image', 'image_alt')) {
    if (!(opcional %in% attributes)) {
      table[, opcional] <- ''
    }
  }
  rest <- setdiff(attributes, c("type", "question", "image", "image_alt", "answer"))
  for (i in 1:nrow(table)) {
    text <- paste0(
      "define_question(qc, type = '",
      table[i, 'type'],
      "', question = '",
      table[i, 'question'],
      "', image = '",
      table[i, 'image'],
      "', image_alt = '",
      table[i, 'image_alt'],
      "', answer = ",
      string_to_string_vector(table[i, 'answer'][[1]])
    )
    j <- 0
    for (r in rest) {
      if (table[i, r][[1]] != '') {
        j <- j + 1
        text <- paste0(text,
                       ", a_", j, " = ", string_to_string_vector(table[i, r][[1]]))
      }
    }
    text <- paste0(text, ")")
    qc <- eval(parse(text = text))
  }
  qc
}
