#' Determine question type
#'
#' Determines the type of the question based on its content, provided answers,
#' and optional type parameter.
#'
#' @param type A string, suggested type for the question.
#' @param question A string, the main question text.
#' @param answer A string or vector, the correct answer(s) for the question.
#' @param a_values A vector, additional answers for the question.
#'
#' @return A string indicating the question type.
#' @keywords internal
determine_question_type <- function(type, question, answer, a_values) {
  if (length(answer) == 3) {
    'ddmarker'
  } else if (is_numeric(answer)) {
    'numerical'
  } else if (length(a_values) > 0) {
    if (length(answer) == 1) {
      if (!has_gaps(question)) {
        if (type == '') {
          'multichoice'
        } else if (type == 'h') {
          'ordering<|>h'
        } else {
          'ordering<|>v'
        }
      } else if (type == '') {
        'ddwtos'
      } else {
        'gapselect'
      }
    } else {
      'matching'
    }
  } else if (answer == '') {
    'essay'
  } else if (tolower(answer) %in% c('true', 'false')) {
    'truefalse'
  } else {
    'shortanswer'
  }
}
