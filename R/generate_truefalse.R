

#' Generate `truefalse` question
#'
#' @param answer A string.
#'
#' @return A string.
#' @keywords internal
generate_truefalse <- function(answer) {
  answer <- tolower(answer)
  if (answer == 'true') {
    answer_2 <- 'false'
  } else {
    answer_2 <- 'true'
  }

  question_body <- glue::glue(
    '

    <defaultgrade>1.0000000</defaultgrade>
    <penalty>1.0000000</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <answer fraction="100" format="moodle_auto_format">
      <text>{answer}</text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="moodle_auto_format">
      <text>{answer_2}</text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
'
  )

  question_body
}
