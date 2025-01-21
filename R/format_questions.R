
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
    w <- trimws(others[[s]])
    if (length(w) > 1) {
      w <- vector_to_string(w)
    }
    if (nchar(w) > 0) {
      a_values <- c(a_values, w)
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
#' @param fb_answer A string, answer feedback.
#' @param fb_a_values A vector, rest of answer feedback.
#' @param image Path to an image file associated with the question.
#' @param image_alt Alternative text describing the image for accessibility.
#' @param fraction A number between 0 and 1.
#'
#' @return A string containing the question body in XML format.
#' @keywords internal
generate_question_body <- function(type,
                                   answer,
                                   a_values,
                                   fb_correct,
                                   fb_incorrect,
                                   fb_partially,
                                   orientation,
                                   fb_answer,
                                   fb_a_values,
                                   image,
                                   image_alt,
                                   fraction) {
  switch(
    type,
    "numerical" = generate_numerical(answer, a_values, fb_answer, fb_a_values),
    "multichoice" = generate_multichoice(
      answer,
      a_values,
      fb_correct,
      fb_incorrect,
      fb_partially,
      fb_answer,
      fb_a_values,
      fraction
    ),
    "ordering" = generate_ordering(
      answer,
      a_values,
      fb_correct,
      fb_incorrect,
      fb_partially,
      orientation
    ),
    "ddwtos" = generate_ddwtos(answer, a_values, fb_correct, fb_incorrect, fb_partially),
    "gapselect" = generate_gapselect(answer, a_values, fb_correct, fb_incorrect, fb_partially),
    "matching" = generate_matching(answer, a_values, fb_correct, fb_incorrect, fb_partially),
    "essay" = generate_essay(),
    "truefalse" = generate_truefalse(answer, fb_answer, fb_a_values, fraction),
    "shortanswer" = generate_shortanswer(answer, fb_answer),
    "ddmarker" = generate_ddmarker(image, image_alt, answer, a_values)
  )
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
