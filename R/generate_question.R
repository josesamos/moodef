#' Generate question
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param copyright A string, copyright text to be included in each question that
#' is defined.
#' @param license A string, license text to be included in each question that is
#' defined.
#' @param correct_feedback A string, feedback on correct answers to each question.
#' @param partially_correct_feedback A string, feedback on partially correct answers
#' to each question.
#' @param incorrect_feedback A string, feedback on incorrect answers to each question.
#' @param adapt_images A boolean, adapt the images so that they are a similar size.
#' @param width A integer, width of each image.
#' @param height A integer, height of each image.
#' @param type A string, question type (if needed).
#' @param question A string, statement of the question.
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#' @param answer A string, correct answer to the question.
#' @param ... A string, rest of the answers to the question.
#'
#' @return A string.
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

  # only no empty elements
  others <- list(...)
  rest <- NULL
  for (s in seq_along(others)) {
    ot <- trimws(others[[s]])
    if (nchar(ot) > 0) {
      rest <- c(rest, ot)
    }
  }
  n <- length(rest)

  answer <- string_to_vector(answer)
  if (is.null(answer)) {
    answer <- ''
  }

  orientation <- ''

  if (is_numeric(answer)) {
    type <- 'numerical'
    question_body <- generate_numerical(answer,rest)
  } else {
    if (n > 0) {
      if (length(answer) == 1) {
        if (!has_gaps(question)) {
          if (type == '') {
            type <- 'multichoice'
            question_body <- generate_multichoice(answer,
                                                  rest,
                                                  correct_feedback,
                                                  incorrect_feedback)
          } else {
            if (type == 'H' | type == 'h') {
              orientation <- 'h'
            }else {
              orientation <- 'v'
            }
            type <- 'ordering'
            question_body <- generate_ordering(
              answer,
              rest,
              correct_feedback,
              incorrect_feedback,
              partially_correct_feedback,
              orientation
            )
          }
        } else {
          if (type == '') {
            type <- 'ddwtos'
            question_body <- generate_ddwtos(
              answer,
              rest,
              correct_feedback,
              incorrect_feedback,
              partially_correct_feedback
            )
          } else {
            type <- 'gapselect'
            question_body <- generate_gapselect(
              answer,
              rest,
              correct_feedback,
              incorrect_feedback,
              partially_correct_feedback
            )
          }
        }
      } else {
        type <- 'matching'
        question_body <- generate_matching(
          answer,
          rest,
          correct_feedback,
          incorrect_feedback,
          partially_correct_feedback
        )
      }
    } else {
      if (answer == '') {
        type <- 'essay'
        question_body <- generate_essay()
      } else {
        value <- tolower(answer)
        if (value %in% c('true', 'false')) {
          type <- 'truefalse'
          question_body <- generate_truefalse(answer)
        } else {
          type <- 'shortanswer'
          question_body <- generate_shortanswer(answer)
        }
      }
    }
  }

  questiontext <- xml_questiontext(copyright,
                                   license,
                                   adapt_images,
                                   width,
                                   height,
                                   question,
                                   image,
                                   image_alt,
                                   type)
  name <- generate_name(first_question_number, type, orientation, question)

  xml_question(type, name, questiontext, question_body)
}
