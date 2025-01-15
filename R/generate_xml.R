
#' Define the question in xml
#'
#' @param type A string, question type.
#' @param name A string, question name.
#' @param questiontext A string, question text.
#' @param question_body A string, question body.
#' @param question_body A string, question tags.
#'
#' @return A string.
#' @keywords internal
xml_question <- function (type, name, questiontext, question_body, question_tags = '') {
  glue::glue(
    '

<question type="{type}">
  {name}
  {questiontext}
  {question_body}
  {if (question_tags != "") question_tags else ""}
</question>
'
  )
}


#' Define the question tags in xml
#'
#' @param tag_values A string, tag values.
#'
#' @return A string.
#' @keywords internal
xml_question_tags <- function (tag_values) {
  if (is.null(tag_values) || length(tag_values) == 0) {
    return('')
  } else {
    tags <- paste0("    <tag><text>", tag_values, "</text></tag>", collapse = "\n")
    xml <- paste0("<tags>\n", tags, "\n  </tags>")
    return(xml)
  }
}


#' Define the question category in xml
#'
#' @param category A string, category name.
#'
#' @return A string.
#' @keywords internal
xml_question_category <- function (category) {
  glue::glue(
    '

  <question type="category">
    <category> <text>$course$/top/{category}</text> </category>
    <info format="html"> <text></text> </info>
    <idnumber></idnumber>
  </question>
'
  )
}

#' Generate HTML and XML Representations for an Image
#'
#' This function generates an HTML `<img>` tag and an XML `<file>` tag for a given image.
#' The image can be processed to adjust its dimensions and encoded in base64 for embedding.
#'
#' @param image `character`
#'   Path to the image file. If the string is empty, the function returns empty values.
#' @param image_alt `character`
#'   Alternative text for the image, used for accessibility purposes (e.g., screen readers).
#' @param adapt_images `logical`, default `FALSE`
#'   If `TRUE`, the image dimensions are adjusted to the specified `width` and `height`.
#'   If `FALSE`, the original dimensions are used.
#' @param width `numeric` or `NULL`, default `NULL`
#'   Desired width for the image. Only used when `adapt_images = TRUE`. If `NULL`,
#'   the width is derived from the image file.
#' @param height `numeric` or `NULL`, default `NULL`
#'   Desired height for the image. Only used when `adapt_images = TRUE`. If `NULL`,
#'   the height is derived from the image file.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`img`}{`character`: HTML string containing the `<img>` tag.}
#'   \item{`fimg`}{`character`: XML string containing the base64-encoded `<file>` tag.}
#' }
#'
#' @details
#' - If `adapt_images = TRUE`, the function resizes the image using the specified dimensions.
#' - If `adapt_images = FALSE`, the function reads the image's original dimensions and uses them.
#' - The image is embedded as a base64 string in the `<file>` tag for compatibility with XML-based systems.
#' @keywords internal
xml_image <- function(image, image_alt, adapt_images = FALSE, width = NULL, height = NULL) {
  image <- trimws(image)

  if (nchar(image) > 0) {
    image_alt <- trimws(image_alt)
    file <- basename(image)

    if (adapt_images) {
      image <- adapt_image(image_file = image, width = width, height = height)
    } else {
      fig <- magick::image_read(image)
      inf <- magick::image_info(fig)
      width <- inf$width
      height <- inf$height
    }

    f <- blastula::add_image(image)
    h <- xml2::read_html(f)
    v <- xml2::xml_find_first(h, ".//img")
    s <- xml2::xml_attr(v, "src")
    pos <- unlist(gregexpr(",", s))[1]
    value <- substr(s, pos + 1, nchar(s))

    img <- glue::glue(
      '<p><img src="@@PLUGINFILE@@/{file}" alt="{image_alt}" width="{width}" height="{height}" class="img-fluid atto_image_button_text-bottom"></p>'
    )
    fimg <- glue::glue('<file name="{file}" path="/" encoding="base64">{value}</file>')
  } else {
    img <- ""
    fimg <- ""
  }

  # Return a list with img and fimg
  list(img = img, fimg = fimg)
}


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
#' @param type A string, question type.
#' @param author A string, author name to be included in each question that is defined.
#' @param fb_general A string, general feedback to be included in each question that is defined.
#' @param idnumber A string, idnumber to be included in each question that is defined.
#'
#' @return A string.
#' @keywords internal
xml_questiontext <- function(copyright,
                             license,
                             adapt_images,
                             width,
                             height,
                             question,
                             image,
                             image_alt,
                             type,
                             author = '',
                             fb_general = '',
                             idnumber = '') {
  # ddmarker image is out of question text
  if (type != "ddmarker") {
    result <- xml_image(image, image_alt, adapt_images, width, height)
    img <- result$img
    fimg <- result$fimg
  } else {
    img <- ""
    fimg <- ""
  }

  defaultgrade <- "1.0"
  penalty <- "0.3333333"
  if (type == 'essay') {
    penalty <- "0"
  } else if (type == 'multichoice') {
    penalty <- "0.5"
  } else if (type == 'truefalse') {
    penalty <- "1.0"
  }

  questiontext <- glue::glue(
    '

    <questiontext format="html">
      <text><![CDATA[
         {if (copyright != "") paste0("<!-- ", copyright, " -->") else ""}
         {if (license != "") paste0("<!-- ", license, " -->") else ""}
         {if (author != "") paste0("<!-- Author: ", author, " -->") else ""}
         <p>{question}</p>{img}]]></text>
         {fimg}
    </questiontext>
    <generalfeedback format="html">
      <text>{if (fb_general != "") paste0("<![CDATA[<p>", fb_general, "</p>]]>") else ""}</text>
    </generalfeedback>
    <defaultgrade>{defaultgrade}</defaultgrade>
    <penalty>{penalty}</penalty>
    <hidden>0</hidden>
    <idnumber>{idnumber}</idnumber>
'
  )
  questiontext
}


#' Define the question name in xml
#'
#' @param name A string, question name.
#'
#' @return A string.
#' @keywords internal
xml_question_name <- function (name) {
  glue::glue(
    '
<name> <text>{name}</text> </name>
'
  )
}


#' Define the question id number in xml
#'
#' @param idnumber A string, question id number.
#'
#' @return A string.
#' @keywords internal
xml_question_idnumber <- function (idnumber) {
  glue::glue(
    '

  <idnumber>{idnumber}</idnumber>
'
  )
}


#' generate `name` node
#'
#' @param first_question_number An integer, first number to compose the question
#' names.
#' @param type A string, question type (if needed).
#' @param orientation A string, 'h' or 'v'.
#' @param question A string, statement of the question.
#'
#' @return A string.
#' @keywords internal
generate_name <-
  function(first_question_number,
           type,
           orientation,
           question) {
    name <-
      sprintf("q%03d_%s_%s_%s",
              first_question_number,
              type,
              orientation,
              substr(question, 1, 40))
    name <- snakecase::to_snake_case(name)
    xml_question_name(name)
  }


#' Define the category of questions
#'
#' @param category A string, category name.
#' @param questions A string, formatted questions.
#'
#' @return A string.
#' @keywords internal
category_question <- function(category, questions) {
  glue::glue(
    '<?xml version="1.0" encoding="UTF-8"?>
<quiz>',
    xml_question_category(category),
    questions,
    '
</quiz>'
  )
}



###########################################

#' Generate questions xml string
#'
#' @param qc A `question_category` object.
#'
#' @return A string.
#'
#' @family question definition
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test') |>
#'   define_question(
#'     question = 'What are the basic arithmetic operations?',
#'     answer = 'Addition, subtraction, multiplication and division.',
#'     a_1 = 'Addition and subtraction.',
#'     a_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   )
#'
#' xml <- qc |>
#'   generate_xml()
#'
#' @export
generate_xml <- function(qc)
  UseMethod("generate_xml")

#' @rdname generate_xml
#' @export
generate_xml.question_category <- function(qc) {
  if (is.null(qc$extended)) {
    questions <- format_questions(qc$questions)
    xml <- category_question(qc$category, questions)
  } else {
    xml <- extended_format_questions(qc)
  }
  xml
}


#' Generate questions xml file
#'
#' @param qc A `question_category` object.
#' @param file A string, file name.
#'
#' @return A `question_category`.
#'
#' @family question definition
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test') |>
#'   define_question(
#'     question = 'What are the basic arithmetic operations?',
#'     answer = 'Addition, subtraction, multiplication and division.',
#'     a_1 = 'Addition and subtraction.',
#'     a_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   ) |>
#'   generate_xml_file(file = tempfile(fileext = '.xml'))
#'
#' @export
generate_xml_file <- function(qc, file)
  UseMethod("generate_xml_file")

#' @rdname generate_xml_file
#' @export
generate_xml_file.question_category <- function(qc, file = NULL) {
  xml <- generate_xml(qc)
  cat(xml, file = file)
  qc
}
