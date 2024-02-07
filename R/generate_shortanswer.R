

#' Generate `shortanswer` question
#'
#' @param answer A string.
#'
#' @return A string.
#' @keywords internal
generate_shortanswer <- function(answer) {
  question_body <- glue::glue(
    '

    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <usecase>0</usecase>
    <answer fraction="100" format="moodle_auto_format">
      <text>{answer}</text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
'
  )

  question_body
}
