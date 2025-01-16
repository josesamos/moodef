
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
  rest <- filter_non_empty_answers(...)

  answer <- get_vector_answer(answer)

  orientation <- determine_orientation(type)
  type <- determine_question_type(type, question, answer, rest)
  question_body <- generate_question_body(type, answer, rest, correct_feedback,
                                          incorrect_feedback, partially_correct_feedback, orientation)

  questiontext <- xml_questiontext(copyright, license, adapt_images, width, height,
                                   question, image, image_alt, type)

  name <- generate_question_name(first_question_number, type, "", question)

  xml_question(type, name, questiontext, question_body)
}


#' generate question `name` node
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param type A string, question type (if needed).
#' @param orientation A string, 'h' or 'v'.
#' @param question A string, statement of the question.
#'
#' @return A string.
#' @keywords internal
generate_question_name <-
  function(first_question_number,
           type,
           orientation,
           question) {
    name <-
      sprintf("q%03d_%s_%s_%s",
              first_question_number,
              type,
              orientation,
              substr(question, 1, 40))
    name <- snakecase::to_snake_case(name)
    xml_question_name(name)
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
  rest <- NULL
  for (s in seq_along(others)) {
    ot <- trimws(others[[s]])
    if (nchar(ot) > 0) {
      rest <- c(rest, ot)
    }
  }
  rest
}


#' Generate the question body
#'
#' Creates the body of the question based on its type and additional parameters
#' such as feedback and orientation.
#'
#' @param type A string, the question type.
#' @param answer A string or vector, the correct answer(s) for the question.
#' @param rest A vector, additional answers for the question.
#' @param correct_feedback A string, feedback for correct answers.
#' @param incorrect_feedback A string, feedback for incorrect answers.
#' @param partially_correct_feedback A string, feedback for partially correct answers.
#' @param orientation A char, 'h' or 'v'.
#'
#' @return A string containing the question body in XML format.
#' @keywords internal
generate_question_body <- function(type, answer, rest, correct_feedback,
                                   incorrect_feedback, partially_correct_feedback, orientation) {
  switch(type,
         "numerical" = generate_numerical(answer, rest),
         "multichoice" = generate_multichoice(answer, rest, correct_feedback, incorrect_feedback),
         "ordering" = generate_ordering(answer, rest, correct_feedback, incorrect_feedback, partially_correct_feedback, orientation),
         "ddwtos" = generate_ddwtos(answer, rest, correct_feedback, incorrect_feedback, partially_correct_feedback),
         "gapselect" = generate_gapselect(answer, rest, correct_feedback, incorrect_feedback, partially_correct_feedback),
         "matching" = generate_matching(answer, rest, correct_feedback, incorrect_feedback, partially_correct_feedback),
         "essay" = generate_essay(),
         "truefalse" = generate_truefalse(answer),
         "shortanswer" = generate_shortanswer(answer))
}


#' Determine Orientation
#'
#' Determines the orientation based on the input type.
#' If the type is "H" or "h", the orientation will be horizontal ("h").
#' Otherwise, the orientation will be vertical ("v").
#'
#' @param type A character string, typically "H", "h", or any other value.
#'   - "H" or "h": The orientation will be "h" (horizontal).
#'   - Any other value: The orientation will be "v" (vertical).
#'
#' @return A character string: either "h" for horizontal or "v" for vertical.
#'
#' @keywords internal
determine_orientation <- function(type) {
  if (type == 'H' | type == 'h') {
    orientation <- 'h'
  }else {
    orientation <- 'v'
  }
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
