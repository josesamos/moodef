
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
  fq <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
<quiz>
')

  for (i in seq_len(nrow(qc$questions))) {
    question_category <- xml_question_category(qc$questions[i, "category"])

    # "ordering", "ordering<|>h", "ordering<|>v"
    r <- extract_type_orientation(qc$questions[["type"]][i])
    type <- r$type
    orientation <- r$orientation

    author <- qc$questions[["author"]][i]
    penalty <- qc$questions[["penalty"]][i]
    idnumber <- qc$questions[["id"]][i]
    fb_general <- qc$questions[["fb_general"]][i]
    fb_correct <- qc$questions[["fb_correct"]][i]
    fb_incorrect <- qc$questions[["fb_incorrect"]][i]
    fb_partially <- qc$questions[["fb_partially"]][i]
    question <- qc$questions[["question"]][i]
    image <- qc$questions[["image"]][i]
    image_alt <- qc$questions[["image_alt"]][i]
    name <- xml_question_name(qc$questions[i, "name"])
    answer <- get_vector_answer(qc$questions[["answer"]][i])
    a_values <- get_non_empty_fields_by_prefix(qc$questions, i, "a_")
    fb_answer <- qc$questions[["fb_answer"]][i]
    fb_a_values <- get_non_empty_fields_by_prefix(qc$questions, i, "fb_a_")

    questiontext <- xml_questiontext(
      qc$copyright,
      qc$license,
      qc$adapt_images,
      qc$width,
      qc$height,
      question,
      image,
      image_alt,
      type,
      author,
      fb_general,
      idnumber
    )

    question_body <- generate_question_body(
      type,
      answer,
      a_values,
      fb_correct,
      fb_incorrect,
      fb_partially,
      orientation,
      fb_answer,
      fb_a_values,
      image,
      image_alt
    )

    tag_values <- get_non_empty_fields_by_prefix(qc$questions, i, "tag_")
    question_tags <- xml_question_tags(tag_values)

    question <- xml_question(type, name, questiontext, question_body, question_tags)
    fq <- glue::glue(fq, question_category, question)
  }

  fq <- glue::glue(fq, '
</quiz>
')
  fq
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


