

#' Generate `ddmarker` question
#'
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#' @param answer A string vector.
#' @param fb_answer A string, answer feedback.
#'
#' @return A string.
#' @keywords internal
generate_ddmarker <- function(image, image_alt, answer, fb_answer = '') {

  result <- xml_image(image, image_alt)
  fimg <- result$fimg
  i <- 1

  question_body <- glue::glue(
    '

    {fimg}
    <drag>
      <no>{i}</no>
      <text>{answer[3]}</text>
      <noofdrags>{i}</noofdrags>
    </drag>
    <drop>
      <no>{i}</no>
      <shape>{answer[1]}</shape>
      <coords>{answer[2]}</coords>
      <choice>{i}</choice>
    </drop>
'
  )

  for (r in fb_answer) {
    r <- string_to_vector(r)
    i <- i + 1

    question_body <- glue::glue(question_body,
      '

    <drag>
      <no>{i}</no>
      <text>{r[3]}</text>
      <noofdrags>{i}</noofdrags>
    </drag>
    <drop>
      <no>{i}</no>
      <shape>{r[1]}</shape>
      <coords>{r[2]}</coords>
      <choice>{i}</choice>
    </drop>
'
    )
  }

  question_body
}
