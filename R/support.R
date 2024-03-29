
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
  if (nrow(df) == 1) {
    df[, attributes] <-
      tibble::as_tibble(as.list(apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))))
  } else {
    df[, attributes] <-
      apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))
  }
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  df
}



#' Create a question Excel file
#'
#' Creates an empty question Excel file.
#'
#' @param file A string, name of a text file.
#'
#' @family support functions
#'
#' @examples
#'
#' \donttest{
#' file <- create_question_excel(file = tempfile(fileext = '.xlsx'))
#' }
#'
#' @return A string.
#' @export
create_question_excel <- function(file) {
  questions <- create_question_data_frame()
  xlsx::write.xlsx(
    as.data.frame(questions),
    file = file,
    sheetName = 'moodef',
    row.names = FALSE,
    showNA = FALSE
  )
  invisible(file)
}


#' Read a question Excel file
#'
#' Reads an Excel file of questions and returns a data frame.
#'
#' In addition to the file, we can indicate the sheet by its name or index. If we
#' do not indicate anything, it considers the first sheet.
#'
#' @param file A string, name of a text file.
#' @param sheet_index A number, sheet index in the workbook.
#' @param sheet_name A string, sheet name.
#'
#' @family support functions
#'
#' @examples
#'
#' \donttest{
#' file <- system.file("extdata", "questions.xlsx", package = "moodef")
#' df <- read_question_excel(file = file)
#' }
#'
#' @return A data frame.
#' @export
read_question_excel <- function(file,
                                sheet_index = NULL,
                                sheet_name = NULL) {
  if (is.null(sheet_index) & is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)
  } else if (is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)[sheet_index]
  }
  sheet_name <- sheet_name[1]
  df <- suppressMessages(
    readxl::read_excel(
      file,
      sheet = sheet_name,
      col_names = TRUE,
      col_types = "text",
      trim_ws = TRUE
    )
  )
  attributes <- names(df)
  df[, attributes] <- data.frame(lapply(df[, attributes], as.character), stringsAsFactors = FALSE)
  if (nrow(df) == 1) {
    df[, attributes] <-
      tibble::as_tibble(as.list(apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))))
  } else {
    df[, attributes] <-
      apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))
  }
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  df
}
