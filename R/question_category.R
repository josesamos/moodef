#' `question_category` S3 class
#'
#' Creates a `question_category` object.
#'
#' Defines a category of questions to be included in the *Moodle* question bank.
#'
#' @param category A string, category name.
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
           copyright = NULL,
           license = NULL,
           correct_feedback = 'Correct.',
           partially_correct_feedback = 'Partially correct.',
           incorrect_feedback = 'Incorrect.',
           adapt_images = TRUE,
           width = 800,
           height = 600) {
    questions <-  data.frame(
      copyright = character(),
      license = character(),
      correct_feedback = character(),
      partially_correct_feedback = character(),
      incorrect_feedback = character(),
      adapt_images = logical(),
      width = integer(),
      height = integer(),
      question = character(),
      image = character(),
      alt = character(),
      answer = character(),
      wrong_1 = character(),
      wrong_2 = character(),
      wrong_3 = character(),
      stringsAsFactors = FALSE
    )

    structure(
      list(
        category = category,
        copyright = copyright,
        license = license,
        correct_feedback = correct_feedback,
        partially_correct_feedback = partially_correct_feedback,
        incorrect_feedback = incorrect_feedback,
        adapt_images = adapt_images,
        width = width,
        height = height,
        wrong_n = 3,
        questions = questions
      ),
      class = "question_category"
    )
  }


#' Define a question from the category
#'
#' Define the question and the possible answers. The type of question is deduced.
#'
#' @param qc A `question_category` object.
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
                            question,
                            image,
                            image_alt,
                            answer,
                            ...)
  UseMethod("define_question")


#' @rdname define_question
#' @export
define_question.question_category <- function(qc,
                                              question = '',
                                              image = '',
                                              image_alt = '',
                                              answer = '',
                                              ...) {
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
    copyright = qc$copyright,
    license = qc$license,
    correct_feedback = qc$correct_feedback,
    partially_correct_feedback = qc$partially_correct_feedback,
    incorrect_feedback = qc$incorrect_feedback,
    adapt_images = qc$adapt_images,
    width = qc$width,
    height = qc$height,
    question = question,
    image = image,
    alt = image_alt,
    answer = answer
  )
  if (n > 0) {
    for (i in 1:n) {
      nq[1, paste0('wrong_', i)] <- wrong[i]
    }
  }
  if (n < qc$wrong_n) {
    for (i in (n + 1):qc$wrong_n) {
      nq[1, paste0('wrong_', i)] <- ''
    }
  }
  if (n > qc$wrong_n) {
    for (i in (qc$wrong_n + 1):n) {
      qc$questions[, paste0('wrong_', i)] <- ''
    }
    qc$wrong_n <- n
  }
  qc$questions <- rbind(qc$questions, nq)
  qc
}
