

#' Generate `multichoice` question
#'
#' @param answer A string.
#' @param n An integer, number or answers.
#' @param rest A vector, rest of answers.
#' @param correct_feedback A string.
#' @param incorrect_feedback A string.
#'
#' @return A string.
#' @keywords internal
generate_multichoice <-
  function(answer,
           n,
           rest,
           correct_feedback,
           incorrect_feedback) {
    question <- glue::glue(
      '

    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.5</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <single>true</single>
    <shuffleanswers>true</shuffleanswers>
    <answernumbering>abc</answernumbering>
    <showstandardinstruction>0</showstandardinstruction>
    <correctfeedback format="moodle_auto_format"> <text>{correct_feedback}</text> </correctfeedback>
    <partiallycorrectfeedback format="moodle_auto_format"> <text></text> </partiallycorrectfeedback>
    <incorrectfeedback format="moodle_auto_format"> <text>{incorrect_feedback}</text> </incorrectfeedback>
    <answer fraction="100" format="html">
       <text>{answer}</text>
       <feedback format="html"> <text>{correct_feedback}</text> </feedback>
    </answer>
'
    )

    value <- sprintf("-%2.15f", 100 / n)
    others <- NULL
    for (r in rest) {
      others <- paste0 (
        others,
        glue::glue(
          '

    <answer fraction="{value}" format="html">
       <text>{r}</text>
       <feedback format="html"> <text>{incorrect_feedback}</text> </feedback>
    </answer>
'
        )
      )
    }

    question_body <- paste0(question, others)
    question_body
  }
