
#' Format all questions in the data frame
#'
#' @param questions A question data frame.
#'
#' @return A string.
#' @keywords internal
format_questions <- function(questions) {
  paste(unlist(purrr::pmap(questions, generate_question)), collapse = "\n")
}


#' Generate a question
#'
#' Main function to generate a question in XML format. Combines various components
#' such as type, text, and body to construct the complete XML question.
#'
#' @param first_question_number An integer, the first number to compose the question names.
#' @param copyright A string, copyright text to include in the question.
#' @param license A string, license text to include in the question.
#' @param correct_feedback A string, feedback for correct answers.
#' @param partially_correct_feedback A string, feedback for partially correct answers.
#' @param incorrect_feedback A string, feedback for incorrect answers.
#' @param adapt_images A boolean, whether to adapt images for size consistency.
#' @param width An integer, width of the images.
#' @param height An integer, height of the images.
#' @param type A string, question type (if applicable).
#' @param question A string, the main question text.
#' @param image A string, optional image file to include in the question.
#' @param image_alt A string, description of the image for accessibility.
#' @param answer A string, correct answer for the question.
#' @param ... Additional strings, other possible answers.
#'
#' @return A string containing the question in XML format.
#' @keywords internal
generate_question <- function(first_question_number,
                              copyright,
                              license,
                              correct_feedback,
                              partially_correct_feedback,
                              incorrect_feedback,
                              adapt_images,
                              width,
                              height,
                              type,
                              question,
                              image,
                              image_alt,
                              answer,
                              ...) {
  a_values <- filter_non_empty_answers(...)

  answer <- get_vector_answer(answer)

  if (!(type %in% allowed_types)) {
    type <- determine_question_type(type, question, answer, a_values)
  }
  # "ordering", "ordering<|>h", "ordering<|>v"
  r <- extract_type_orientation(type)
  type <- r$type
  orientation <- r$orientation

  question_body <- generate_question_body(type, answer, a_values, correct_feedback,
                                          incorrect_feedback, partially_correct_feedback, orientation)

  questiontext <- xml_questiontext(copyright, license, adapt_images, width, height,
                                   question, image, image_alt, type)

  name <- generate_question_name(first_question_number, type, question)
  question_name <- xml_question_name(name)

  xml_question(type, question_name, questiontext, question_body)
}


#' generate question `name` node
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param type A string, question type (if needed).
#' @param question A string, statement of the question.
#'
#' @return A string.
#' @keywords internal
generate_question_name <- function(first_question_number, type, question) {
    name <-
      sprintf("q%03d_%s_%s",
              first_question_number,
              type,
              substr(question, 1, 40))
    snakecase::to_snake_case(name)
  }


#' Filter non-empty answers
#'
#' Filters additional answers provided in `...` to ensure only non-empty answers
#' are included in the final output.
#'
#' @param ... Strings, additional answers.
#'
#' @return A vector of non-empty answers.
#' @keywords internal
filter_non_empty_answers <- function(...) {
  others <- list(...)
  a_values <- NULL
  for (s in seq_along(others)) {
    ot <- trimws(others[[s]])
    if (nchar(ot) > 0) {
      a_values <- c(a_values, ot)
    }
  }
  a_values
}


#' Generate the question body
#'
#' Creates the body of the question based on its type and additional parameters
#' such as feedback and orientation.
#'
#' @param type A string, the question type.
#' @param answer A string or vector, the correct answer(s) for the question.
#' @param a_values A vector, additional answers for the question.
#' @param fb_correct A string, feedback for correct answers.
#' @param fb_incorrect A string, feedback for incorrect answers.
#' @param fb_partially A string, feedback for partially correct answers.
#' @param orientation A string, 'v' or 'h'.
#'
#' @return A string containing the question body in XML format.
#' @keywords internal
generate_question_body <- function(type, answer, a_values, fb_correct,
                                   fb_incorrect, fb_partially, orientation,
                                   fb_answer = '', fb_a_values = NULL,
                                   image = '', image_alt = '') {
  switch(type,
         "numerical" = generate_numerical(answer, a_values, fb_answer, fb_a_values),
         "multichoice" = generate_multichoice(answer, a_values, fb_correct, fb_incorrect, fb_partially, fb_answer, fb_a_values),
         "ordering" = generate_ordering(answer, a_values, fb_correct, fb_incorrect, fb_partially, orientation),
         "ddwtos" = generate_ddwtos(answer, a_values, fb_correct, fb_incorrect, fb_partially),
         "gapselect" = generate_gapselect(answer, a_values, fb_correct, fb_incorrect, fb_partially),
         "matching" = generate_matching(answer, a_values, fb_correct, fb_incorrect, fb_partially),
         "essay" = generate_essay(),
         "truefalse" = generate_truefalse(answer, fb_answer, fb_a_values),
         "shortanswer" = generate_shortanswer(answer, fb_answer),
         "ddmarker" = generate_ddmarker(image, image_alt, answer, a_values))
}


#' Convert Answer String to Vector
#'
#' Converts an answer string into a vector using `string_to_vector`.
#' If the input is `NULL`, it returns an empty string.
#'
#' @param answer A character string representing the answer.
#'
#' @return A vector if the answer is successfully converted, or an empty string if the answer is `NULL`.
#' @keywords internal
get_vector_answer <- function(answer) {
  answer <- string_to_vector(answer)
  if (is.null(answer)) {
    ''
  } else {
    answer
  }
}

#' Extract Type and Orientation
#'
#' This function takes a string representing a question type and its optional orientation
#' (e.g., `"ordering<|>h"`) and splits it into two separate components: the type and
#' the orientation. If the orientation is not provided, it defaults to `"v"`.
#'
#' @param type A character string indicating the type of a question, which may optionally
#' include an orientation separated by `"<|>"`. For example: `"ordering<|>h"`,
#' `"ordering<|>v"`, or just `"ordering"`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{type}{The main question type as a character string.}
#'   \item{orientation}{The orientation of the type as a character string. Defaults to `"v"`.}
#' }
#'
#' @keywords internal
extract_type_orientation <- function(type) {
  type_components <- string_to_vector(type)
  orientation <- ifelse(is.na(type_components[2]), "v", type_components[2])
  type <- type_components[1]
  data.frame(type, orientation)
}
