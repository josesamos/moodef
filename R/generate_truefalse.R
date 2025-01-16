

#' Generate `truefalse` question
#'
#' @param answer A string.
#' @param fb_answer A string, answer feedback.
#' @param fb_rest A vector, rest of answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_truefalse <- function(answer, fb_answer = '', fb_rest = NULL) {
  answer <- tolower(answer)
  if (answer == 'true') {
    answer_2 <- 'false'
  } else {
    answer <- 'false'
    answer_2 <- 'true'
  }
  if (is.null(fb_rest)) {
    fb_rest <- ''
  } else {
    fb_rest <- fb_rest[1]
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
        <text>{fb_rest}</text>
      </feedback>
    </answer>
'
  )

  question_body
}
