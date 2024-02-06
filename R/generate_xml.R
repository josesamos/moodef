

generate_questiontext <- function(copyright,
                                  license,
                                  adapt_images,
                                  width,
                                  height,
                                  question,
                                  image,
                                  alt) {
  image <- trimws(image)
  if (nchar(image) > 0) {
    alt <- trimws(alt)
    file <- basename(image)
    if (adapt_images) {
      image <-
        adapt_image(image_file = image,
                    width = width,
                    height = height)
    } else {
      fig <- magick::image_read(image)
      inf <- magick::image_info(fig)
      width <- inf$width
      height <- inf$height
    }
    f <- blastula::add_image(image)
    h <- xml2::read_html(f)
    v <- xml2::xml_find_first(h, ".//img")
    s <- xml2::xml_attr(v, 'src')
    pos <- unlist(gregexpr(',', s))[1]
    value <- substr(s, pos + 1, nchar(s))

    img <-
      glue::glue(
        '<p><img src="@@PLUGINFILE@@/{file}" alt="{alt}" width="{width}" height="{height}" class="img-fluid atto_image_button_text-bottom"></p>'
      )
    fimg <-
      glue::glue('<file name="{file}" path="/" encoding="base64">{value}</file>')
  } else {
    img <- ''
    fimg <- ''
  }

  questiontext <- glue::glue(
    '
    <name> <text>{question}</text> </name>
    <questiontext format="html">
      <text><![CDATA[
         <!-- {copyright} -->
         <!-- {license} -->
         <p>{question}</p>{img}]]></text>
{fimg}
    </questiontext>
    <generalfeedback format="html"> <text></text> </generalfeedback>
                         '
  )
  questiontext
}


generate_question <- function(copyright,
                              license,
                              correct_feedback,
                              partially_correct_feedback,
                              incorrect_feedback,
                              adapt_images,
                              width,
                              height,
                              question,
                              image,
                              alt,
                              answer,
                              ...) {

  questiontext <- generate_questiontext(copyright,
                                        license,
                                        adapt_images,
                                        width,
                                        height,
                                        question,
                                        image,
                                        alt)

  others <- list(...)
  rest <- NULL
  for (s in seq_along(others)) {
    ot <- trimws(others[[s]])
    if (nchar(ot) > 0) {
      rest <- c(rest, ot)
    }
  }
  n <- length(rest)
  if (n > 0) {
    answer <- string_to_vector(answer)
    if (length(answer) == 1) {
      # multichoice
    } else {
      # matching
    }
  } else {
    value <- tolower(answer)
    if (value %in% c('true', 'false')) {
      # truefalse
    } else {
      # shortanswer
    }
  }

}




#' Generar preguntas
#'
#' @param qc
#' @param file
#'
#' @export
generate_xml <- function(qc, file)
  UseMethod("generate_xml")


#' @rdname generate_xml
#' @export
generate_xml.question_category <- function(qc, file = NULL) {
  questions <- format_questions(qc$questions)
  category <- category_question(qc$category, questions)
  cat(category, file = file)
  qc
}
