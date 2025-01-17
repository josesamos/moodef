

#' Generate `numerical` question
#'
#' @param answer A vector of strings.
#' @param a_values A vector, rest of answers.
#' @param fb_answer A string, answer feedback.
#' @param fb_a_values A vector, rest of answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_numerical <- function(answer, a_values, fb_answer, fb_a_values) {
    answer_1 <- answer[1]
    if (length(answer) > 1) {
      answer_2 <- answer[2]
    } else {
      answer_2 <- 0
    }
    question <- glue::glue(
      '

    <answer fraction="100" format="moodle_auto_format">
      <text>{answer_1}</text>
      <feedback format="html">
        <text>{fb_answer}</text>
      </feedback>
      <tolerance>{answer_2}</tolerance>
    </answer>
'
    )

    others <- NULL
    i <- 1
    for (r in a_values) {
      r <- string_to_vector(r)
      answer_1 <- r[1]
      if (length(r) > 1) {
        answer_2 <- r[2]
      } else {
        answer_2 <- 0
      }
      if (!is.null(fb_a_values[i])) {
        fb <- fb_a_values[i]
      } else {
        fb <- ''
      }
      i <- i + 1

      others <- paste0 (
        others,
        glue::glue(
          '
    <answer fraction="100" format="moodle_auto_format">
      <text>{answer_1}</text>
      <feedback format="html">
        <text>{fb}</text>
      </feedback>
      <tolerance>{answer_2}</tolerance>
    </answer>
'
        )
      )
    }

    question_body <- paste0(question, others, '
    <unitgradingtype>0</unitgradingtype>
    <unitpenalty>0.1000000</unitpenalty>
    <showunits>3</showunits>
    <unitsleft>0</unitsleft>')

    question_body
  }
