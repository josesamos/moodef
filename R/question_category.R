#' `question_category` S3 class
#'
#' Creates a `question_category` object.
#'
#' Defines a category of questions to be included in the *Moodle* question bank.
#'
#' It allows us to define the name of the category, the copyright and license
#' literals that will be added to each question, and the feedback literals for
#' correct, partially correct and incorrect questions.
#'
#' Each question can include an image after the text. We can also configure if
#' we want to automatically transform the images so that they have a standard
#' size that we can also indicate.
#'
#' @param category A string, category name.
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
#'
#' @return A `question_category` object.
#'
#' @family question definition
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test')
#'
#' @export
question_category <-
  function(category = 'Default category',
           first_question_number = 1,
           copyright = '',
           license = '',
           correct_feedback = 'Correct.',
           partially_correct_feedback = 'Partially correct.',
           incorrect_feedback = 'Incorrect.',
           adapt_images = FALSE,
           width = 800,
           height = 600) {
    questions <-  data.frame(
      first_question_number = integer(),
      copyright = character(),
      license = character(),
      correct_feedback = character(),
      partially_correct_feedback = character(),
      incorrect_feedback = character(),
      adapt_images = logical(),
      width = integer(),
      height = integer(),
      type = character(),
      question = character(),
      image = character(),
      image_alt = character(),
      answer = character(),
      a_1 = character(),
      a_2 = character(),
      a_3 = character(),
      stringsAsFactors = FALSE
    )

    structure(
      list(
        category = category,
        first_question_number = first_question_number,
        copyright = copyright,
        license = license,
        correct_feedback = correct_feedback,
        partially_correct_feedback = partially_correct_feedback,
        incorrect_feedback = incorrect_feedback,
        adapt_images = adapt_images,
        width = width,
        height = height,
        a_n = 3,
        questions = questions
      ),
      class = "question_category"
    )
  }


#' Define a question from the category
#'
#' Define the question and the possible answers. The type of question is deduced.
#'
#' If we include an image in the question, we must also include text in the `alt`
#' field associated with it.
#'
#' After the correct answer, we can indicate as many answers as we want, if we do
#' not indicate all the parameters, we have to give each answer a parameter name
#' different from the rest of the parameter names.
#'
#' @param qc A `question_category` object.
#' @param type A string, question type (if needed).
#' @param question A string, statement of the question.
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#' @param answer A string, correct answer to the question.
#' @param ... A string, rest of the answers to the question.
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
#'     w_1 = 'Addition and subtraction.',
#'     w_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   )
#'
#' @export
define_question <- function(qc,
                            type,
                            question,
                            image,
                            image_alt,
                            answer,
                            ...)
  UseMethod("define_question")


#' @rdname define_question
#' @export
define_question.question_category <- function(qc,
                                              type = '',
                                              question = '',
                                              image = '',
                                              image_alt = '',
                                              answer = '',
                                              ...) {
  if (image != '') {
    stopifnot('If an image is included, the associated alt field must also be defined.' = image_alt != '')
  }

  others <- list(...)
  wrong <- NULL
  for (s in seq_along(others)) {
    w <- trimws(others[[s]])
    if (length(w) > 1) {
      w <- vector_to_string(w)
    }
    if (nchar(w) > 0) {
      wrong <- c(wrong, w)
    }
  }
  if (length(answer) > 1) {
    answer <- vector_to_string(answer)
  }
  n <- length(wrong)
  nq <- data.frame(
    first_question_number = qc$first_question_number,
    copyright = qc$copyright,
    license = qc$license,
    correct_feedback = qc$correct_feedback,
    partially_correct_feedback = qc$partially_correct_feedback,
    incorrect_feedback = qc$incorrect_feedback,
    adapt_images = qc$adapt_images,
    width = qc$width,
    height = qc$height,
    type = type,
    question = question,
    image = image,
    image_alt = image_alt,
    answer = answer
  )
  if (n > 0) {
    for (i in 1:n) {
      nq[1, paste0('a_', i)] <- wrong[i]
    }
  }
  if (n < qc$a_n) {
    for (i in (n + 1):qc$a_n) {
      nq[1, paste0('a_', i)] <- ''
    }
  }
  if (nrow(qc$questions) > 0) {
    if (n > qc$a_n) {
      for (i in (qc$a_n + 1):n) {
        qc$questions[, paste0('a_', i)] <- ''
      }
      qc$a_n <- n
    }
    qc$questions <- rbind(qc$questions, nq)
  } else {
    if (n > qc$a_n) {
      qc$a_n <- n
    }
    qc$questions <- nq
  }
  qc$first_question_number <- qc$first_question_number + 1
  qc
}
