

#' generate `questiontext` node
#'
#' @param copyright A string, copyright text to be included in each question that
#' is defined.
#' @param license A string, license text to be included in each question that is
#' defined.
#' @param adapt_images A boolean, adapt the images so that they are a similar size.
#' @param width A integer, width of each image.
#' @param height A integer, height of each image.
#' @param question A string, statement of the question.
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#'
#' @return A string.
#' @keywords internal
generate_questiontext <- function(copyright,
                                  license,
                                  adapt_images,
                                  width,
                                  height,
                                  question,
                                  image,
                                  image_alt) {
  image <- trimws(image)
  if (nchar(image) > 0) {
    image_alt <- trimws(image_alt)
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
        '<p><img src="@@PLUGINFILE@@/{file}" alt="{image_alt}" width="{width}" height="{height}" class="img-fluid atto_image_button_text-bottom"></p>'
      )
    fimg <-
      glue::glue('<file name="{file}" path="/" encoding="base64">{value}</file>')
  } else {
    img <- ''
    fimg <- ''
  }

  questiontext <- glue::glue('

    <questiontext format="html">
      <text><![CDATA[
         <!-- {copyright} -->
         <!-- {license} -->
         <p>{question}</p>{img}]]></text>
{fimg}
    </questiontext>
    <generalfeedback format="html"> <text></text> </generalfeedback>

')
  questiontext
}


#' generate `name` node
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param type A string, question type (if needed).
#' @param position A string, 'h' or 'v'.
#' @param question A string, statement of the question.
#'
#' @return A string.
#' @keywords internal
generate_name <- function(first_question_number, type, position, question) {
  name <-
    sprintf("q%03d_%s_%s_%s",
            first_question_number,
            type,
            position,
            substr(question, 1, 20))
  name <- snakecase::to_snake_case(name)
  name <- glue::glue('

   <name> <text>{name}</text> </name>

')
  name
}


#' Generate question
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param copyright A string, copyright text to be included in each question that
#' is defined.
#' @param license A string, license text to be included in each question that is
#' defined.
#' @param correct_feedback A string, feedback on correct answers to each question.
#' @param partially_correct_feedback A string, feedback on partially correct answers
#' to each question.
#' @param incorrect_feedback A string, feedback on incorrect answers to each question.
#' @param adapt_images A boolean, adapt the images so that they are a similar size.
#' @param width A integer, width of each image.
#' @param height A integer, height of each image.
#' @param type A string, question type (if needed).
#' @param question A string, statement of the question.
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#' @param answer A string, correct answer to the question.
#' @param ... A string, rest of the answers to the question.
#'
#' @return A string.
#' @keywords internal
generate_question <- function(first_question_number,
                              copyright,
                              license,
                              correct_feedback,
                              partially_correct_feedback,
                              incorrect_feedback,
                              adapt_images,
                              width,
                              height,
                              type,
                              question,
                              image,
                              image_alt,
                              answer,
                              ...) {

  questiontext <- generate_questiontext(copyright,
                                        license,
                                        adapt_images,
                                        width,
                                        height,
                                        question,
                                        image,
                                        image_alt)

  others <- list(...)
  rest <- NULL
  for (s in seq_along(others)) {
    ot <- trimws(others[[s]])
    if (nchar(ot) > 0) {
      rest <- c(rest, ot)
    }
  }
  n <- length(rest)
  answer <- string_to_vector(answer)
  position = ''
  if (is_numeric(answer)) {
    type <- 'numerical'
  } else {
    if (n > 0) {
      if (length(answer) == 1) {
        if (!has_gaps(question)) {
          if (type == '') {
            type <- 'multichoice'
          } else if (type == 'H' | type == 'h') {
            type <- 'ordering'
            position = 'h'
          } else {
            type <- 'ordering'
            position = 'v'
          }
        } else {
          if (type == '') {
            type <- 'ddwtos'
          } else {
            type <- 'gapselect'
          }
        }
      } else {
        type <- 'matching'
      }
    } else {
      value <- tolower(answer)
      if (value %in% c('true', 'false')) {
        type <- 'truefalse'
      } else {
        type <- 'shortanswer'
      }
    }
  }

  name <- generate_name(first_question_number, type, position, question)

}




#' Generar preguntas
#'
#' @param qc A `question_category` object.
#' @param file A string, file name.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @export
generate_xml <- function(qc, file)
  UseMethod("generate_xml")


#' @rdname generate_xml
#' @export
generate_xml.question_category <- function(qc, file = NULL) {
  # questions <- format_questions(qc$questions)
  # category <- category_question(qc$category, questions)
  # cat(category, file = file)
  qc
}
