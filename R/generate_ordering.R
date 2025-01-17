

#' Generate `ordering` question
#'
#' @param answer A vector of strings.
#' @param a_values A vector, rest of answers.
#' @param correct_feedback A string.
#' @param incorrect_feedback A string.
#' @param partially_correct_feedback A string.
#' @param orientation A string, 'h' or 'v'.
#'
#' @return A string.
#' @keywords internal
generate_ordering <-
  function(answer,
           a_values,
           correct_feedback,
           incorrect_feedback,
           partially_correct_feedback,
           orientation) {
    if (orientation == 'h') {
      orientation <- 'HORIZONTAL'
    } else {
      orientation <- 'VERTICAL'
    }
    question <- glue::glue(
      '

    <layouttype>{orientation}</layouttype>
    <selecttype>ALL</selecttype>
    <selectcount>0</selectcount>
    <gradingtype>ABSOLUTE_POSITION</gradingtype>
    <showgrading>SHOW</showgrading>
    <numberingstyle>none</numberingstyle>
    <correctfeedback format="html">
      <text>{correct_feedback}</text>
    </correctfeedback>
    <partiallycorrectfeedback format="html">
      <text>{partially_correct_feedback}</text>
    </partiallycorrectfeedback>
    <incorrectfeedback format="html">
      <text>{incorrect_feedback}</text>
    </incorrectfeedback>
    <shownumcorrect>1</shownumcorrect>
    <answer fraction="1.0000000" format="moodle_auto_format">
      <text>{answer}</text>
    </answer>
'
    )

    others <- NULL
    i <- 1
    for (r in a_values) {
      i <- i + 1
      fraction <- paste0(i, '.0000000')
      others <- paste0 (
        others,
        glue::glue(
          '

    <answer fraction="{fraction}" format="moodle_auto_format">
      <text>{r}</text>
    </answer>
'
        )
      )
    }

    question_body <- paste0(question, others, '
    <hint format="html">
      <text></text>
    </hint>
    <hint format="html">
      <text></text>
    </hint>')
    question_body
  }
