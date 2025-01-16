

#' Generate `shortanswer` question
#'
#' @param answer A string.
#' @param fb_answer A string, answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_shortanswer <- function(answer, fb_answer = '') {
  question_body <- glue::glue(
    '

    <usecase>0</usecase>
    <answer fraction="100" format="moodle_auto_format">
      <text>{answer}</text>
      <feedback format="html">
        <text>{fb_answer}</text>
      </feedback>
    </answer>
'
  )

  question_body
}
