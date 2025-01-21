
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
#'
#' @return A `question_category`.
#'
#' @family question definition functions
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
define_questions_from_data_frame <- function(qc, df)
  UseMethod("define_questions_from_data_frame")

#' @rdname define_questions_from_data_frame
#' @export
define_questions_from_data_frame.question_category <- function(qc, df) {

  df <- process_question_dataframe(df)

  q_df <- create_default_value_question_df()
  q_df$fraction <- ''

  additional_fields <- setdiff(names(df), names(q_df))
  if (length(additional_fields) > 0) {
    stop(paste("The data frame contains additional fields not allowed:",
               paste(additional_fields, collapse = ", ")))
  }
  # Copy the columns from df
  q_df <- do.call(rbind, replicate(nrow(df), q_df, simplify = FALSE))
  for (col in names(df)) {
    q_df[[col]] <- df[[col]]
  }

  df <- rbind(qc$questions, q_df)
  define_questions_from_df(qc, df)
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
#'
#' @return A `question_category`.
#'
#' @family question definition functions
#'
#' @examples
#'
#' file <- system.file("extdata", "questions.csv", package = "moodef")
#' qc <-
#'   question_category(category = 'Initial test', adapt_images = TRUE) |>
#'   define_questions_from_csv(file = file)
#'
#' @export
define_questions_from_csv <- function(qc, file, sep)
  UseMethod("define_questions_from_csv")

#' @rdname define_questions_from_csv
#' @export
define_questions_from_csv.question_category <- function(qc, file,
                                                        sep = ',') {
  df <- read_question_csv(file, sep)
  define_questions_from_data_frame(qc, df)
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
#'
#' @return A `question_category`.
#'
#' @family question definition functions
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
                                        sheet_name)
  UseMethod("define_questions_from_excel")

#' @rdname define_questions_from_excel
#' @export
define_questions_from_excel.question_category <- function(qc,
                                                          file,
                                                          sheet_index = NULL,
                                                          sheet_name = NULL) {
  df <- read_question_excel(file, sheet_index, sheet_name)
  define_questions_from_data_frame(qc, df)
}
