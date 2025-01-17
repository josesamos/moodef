

#' Generate `truefalse` question
#'
#' @param answer A string.
#' @param fb_answer A string, answer feedback.
#' @param fb_a_values A vector, rest of answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_truefalse <- function(answer, fb_answer, fb_a_values) {
  answer <- tolower(answer)
  if (answer == 'true') {
    answer_2 <- 'false'
  } else {
    answer <- 'false'
    answer_2 <- 'true'
  }
  if (is.null(fb_a_values)) {
    fb_a_values <- ''
  } else {
    fb_a_values <- fb_a_values[1]
  }

  question_body <- glue::glue(
    '

    <answer fraction="100" format="moodle_auto_format">
      <text>{answer}</text>
      <feedback format="html">
        <text>{fb_answer}</text>
      </feedback>
    </answer>
    <answer fraction="0" format="moodle_auto_format">
      <text>{answer_2}</text>
      <feedback format="html">
        <text>{fb_a_values}</text>
      </feedback>
    </answer>
'
  )

  question_body
}
