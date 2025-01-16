

#' Generate `multichoice` question
#'
#' @param answer A string.
#' @param rest A vector, rest of answers.
#' @param correct_feedback A string.
#' @param incorrect_feedback A string.
#' @param fb_partially A string.
#' @param fb_answer A string, answer feedback.
#' @param fb_rest A vector, rest of answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_multichoice <-
  function(answer,
           rest,
           correct_feedback,
           incorrect_feedback,
           fb_partially = '',
           fb_answer = '',
           fb_rest = NULL) {

    if (fb_answer != '') {
      answer_feedback <- fb_answer
    } else {
      answer_feedback <- correct_feedback
    }

    question <- glue::glue(
      '

    <single>true</single>
    <shuffleanswers>true</shuffleanswers>
    <answernumbering>abc</answernumbering>
    <showstandardinstruction>0</showstandardinstruction>
    <correctfeedback format="moodle_auto_format"> <text>{correct_feedback}</text> </correctfeedback>
    <partiallycorrectfeedback format="moodle_auto_format"> <text>{fb_partially}</text> </partiallycorrectfeedback>
    <incorrectfeedback format="moodle_auto_format"> <text>{incorrect_feedback}</text> </incorrectfeedback>
    <answer fraction="100" format="html">
       <text>{answer}</text>
       <feedback format="html"> <text>{answer_feedback}</text> </feedback>
    </answer>
'
    )

    n <- length(rest)

    value <- sprintf("-%2.15f", 100 / n)
    others <- NULL
    i <- 1
    for (r in rest) {
      if (!is.null(fb_rest[i])) {
        fb <- fb_rest[i]
      } else {
        fb <- incorrect_feedback
      }
      i <- i + 1

      others <- paste0 (
        others,
        glue::glue(
          '

    <answer fraction="{value}" format="html">
       <text>{r}</text>
       <feedback format="html"> <text>{fb}</text> </feedback>
    </answer>
'
        )
      )
    }

    question_body <- paste0(question, others)
    question_body
  }
