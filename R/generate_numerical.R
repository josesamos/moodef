

#' Generate `numerical` question
#'
#' @param answer A vector of strings.
#' @param n An integer, number or answers.
#' @param rest A vector, rest of answers.
#'
#' @return A string.
#' @keywords internal
generate_numerical <-
  function(answer,
           n,
           rest) {
    answer_1 <- answer[1]
    if (length(answer) > 1) {
      answer_2 <- answer[2]
    } else {
      answer_2 <- 0
    }
    question <- glue::glue(
      '

    <defaultgrade>1</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <answer fraction="100" format="moodle_auto_format">
      <text>{answer_1}</text>
      <feedback format="html">
        <text></text>
      </feedback>
      <tolerance>{answer_2}</tolerance>
    </answer>
'
    )

    others <- NULL
    for (r in rest) {
      r <- string_to_vector(r)
      answer_1 <- r[1]
      if (length(r) > 1) {
        answer_2 <- r[2]
      } else {
        answer_2 <- 0
      }
      others <- paste0 (
        others,
        glue::glue(
          '

    <answer fraction="100" format="moodle_auto_format">
      <text>{answer_1}</text>
      <feedback format="html">
        <text></text>
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
