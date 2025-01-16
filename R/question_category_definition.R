
#' Define questions from a data frame
#'
#' Each row in the text data frame is interpreted as a question. We only have to define
#' the columns that we are going to use, the rest of the columns are taken by default.
#'
#' For answers where a vector is required, "<|>" is used as a separator of the vector
#' elements.
#'
#' @param qc A `question_category` object.
#' @param df A data frame.
#' @param extended A Boolean, use extended question definition.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @examples
#'
#' file <- system.file("extdata", "questions.csv", package = "moodef")
#' df <- read_question_csv(file = file)
#'
#' qc <-
#'   question_category(category = 'Initial test', adapt_images = TRUE) |>
#'   define_questions_from_data_frame(df)
#'
#' @export
define_questions_from_data_frame <- function(qc, df, extended)
  UseMethod("define_questions_from_data_frame")

#' @rdname define_questions_from_data_frame
#' @export
define_questions_from_data_frame.question_category <- function(qc, df, extended = FALSE) {
  attributes <- names(df)

  for (opcional in c('type', 'image', 'image_alt')) {
    if (!(opcional %in% attributes)) {
      df[, opcional] <- ''
    }
  }

  # type lowercase
  df$type <- tolower(df$type)

  if (extended) {
    for (opcional in c("category", "id", "name", "author", "fb_correct", "fb_partially", "fb_incorrect")) {
      if (!(opcional %in% attributes)) {
        df[, opcional] <- ''
      }
    }
    define_extended_questions_from_data_frame(qc, df)
  } else {
    rest <- setdiff(attributes, c("type", "question", "image", "image_alt", "answer"))
    for (i in 1:nrow(df)) {
      text <- paste0(
        'define_question(qc, type = "',
        df[i, 'type'],
        '", question = "',
        df[i, 'question'],
        '", image = "',
        df[i, 'image'],
        '", image_alt = "',
        df[i, 'image_alt'],
        '", answer = ',
        string_to_string_vector(df[i, 'answer'][[1]])
      )
      j <- 0
      for (r in rest) {
        if (df[i, r][[1]] != '') {
          j <- j + 1
          text <- paste0(text,
                         ", a_", j, " = ", string_to_string_vector(df[i, r][[1]]))
        }
      }
      text <- paste0(text, ")")
      qc <- eval(parse(text = text))
    }
    qc
  }
}



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
#' @param extended A Boolean, use extended question definition.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @examples
#'
#' file <- system.file("extdata", "questions.csv", package = "moodef")
#' qc <-
#'   question_category(category = 'Initial test', adapt_images = TRUE) |>
#'   define_questions_from_csv(file = file)
#'
#' @export
define_questions_from_csv <- function(qc, file, sep, extended)
  UseMethod("define_questions_from_csv")

#' @rdname define_questions_from_csv
#' @export
define_questions_from_csv.question_category <- function(qc, file,
                                                        sep = ',',
                                                        extended = FALSE) {
  df <- read_question_csv(file, sep)
  define_questions_from_data_frame(qc, df, extended)
}




#' Define questions from a Excel file
#'
#' Each row in the Excel file is interpreted as a question. We only have to define
#' the columns that we are going to use, the rest of the columns are taken by default.
#'
#' In addition to the file, we can indicate the sheet by its name or index. If we
#' do not indicate anything, it considers the first sheet.
#'
#' For answers where a vector is required, "<|>" is used as a separator of the vector
#' elements.
#'
#' @param qc A `question_category` object.
#' @param file A string, name of an Excel file.
#' @param sheet_index A number, sheet index in the workbook.
#' @param sheet_name A string, sheet name.
#' @param extended A Boolean, use extended question definition.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @examples
#'
#' \donttest{
#' file <- system.file("extdata", "questions.xlsx", package = "moodef")
#' qc <-
#'   question_category(category = 'Initial test', adapt_images = TRUE) |>
#'   define_questions_from_excel(file = file)
#' }
#'
#' @export
define_questions_from_excel <- function(qc,
                                        file,
                                        sheet_index,
                                        sheet_name,
                                        extended)
  UseMethod("define_questions_from_excel")

#' @rdname define_questions_from_excel
#' @export
define_questions_from_excel.question_category <- function(qc,
                                                          file,
                                                          sheet_index = NULL,
                                                          sheet_name = NULL,
                                                          extended = FALSE) {
  df <- read_question_excel(file, sheet_index, sheet_name)
  define_questions_from_data_frame(qc, df, extended)
}
