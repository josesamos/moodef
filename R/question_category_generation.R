
#' Generate questions xml string
#'
#' @param qc A `question_category` object.
#'
#' @return A string.
#'
#' @family question definition functions
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test') |>
#'   define_question(
#'     question = 'What are the basic arithmetic operations?',
#'     answer = 'Addition, subtraction, multiplication and division.',
#'     a_1 = 'Addition and subtraction.',
#'     a_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   )
#'
#' xml <- qc |>
#'   generate_xml()
#'
#' @export
generate_xml <- function(qc)
  UseMethod("generate_xml")

#' @rdname generate_xml
#' @export
generate_xml.question_category <- function(qc) {
  if (is.null(qc$extended)) {
    questions <- format_questions(qc$questions)
    xml <- category_question(qc$category, questions)
  } else {
    xml <- extended_format_questions(qc)
  }
  xml
}


#' Generate questions xml file
#'
#' @param qc A `question_category` object.
#' @param file A string, file name.
#'
#' @return A `question_category`.
#'
#' @family question definition functions
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test') |>
#'   define_question(
#'     question = 'What are the basic arithmetic operations?',
#'     answer = 'Addition, subtraction, multiplication and division.',
#'     a_1 = 'Addition and subtraction.',
#'     a_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   ) |>
#'   generate_xml_file(file = tempfile(fileext = '.xml'))
#'
#' @export
generate_xml_file <- function(qc, file)
  UseMethod("generate_xml_file")

#' @rdname generate_xml_file
#' @export
generate_xml_file.question_category <- function(qc, file = NULL) {
  xml <- generate_xml(qc)
  cat(xml, file = file)
  qc
}
