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
#' @param author A string, author name to be included in each question that is
#' defined.
#' @param fraction A number between 0 and 1.
#'
#' @return A `question_category` object.
#'
#' @family question definition functions
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
           height = 600,
           author = '',
           fraction = 0) {

    structure(
      list(
        category = category,
        fraction = as.character(fraction),
        first_question_number = first_question_number,
        copyright = copyright,
        license = license,
        author = author,
        correct_feedback = correct_feedback,
        partially_correct_feedback = partially_correct_feedback,
        incorrect_feedback = incorrect_feedback,
        adapt_images = adapt_images,
        width = width,
        height = height,
        questions = create_common_question_df()
      ),
      class = "question_category"
    )
  }


#' Define a question
#'
#' Define a question and the possible answers. The type of question is deduced.
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
#' @family question definition functions
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
  if (length(answer) > 1) {
    answer <- vector_to_string(answer)
  }

  df <- create_default_value_question_df()
  df$type <- type
  df$question <- question
  df$image <- image
  df$image_alt <- image_alt
  df$answer <- answer

  a_values <- filter_non_empty_answers(...)
  n <- length(a_values)
  if (n > 7) {
    warning("No more than 7 alternative answers to the correct one can be indicated.")
    n <- 7
  }
  if (n > 0) {
    for (i in 1:n) {
      df[1, paste0('a_', i)] <- a_values[i]
    }
  }

  df <- rbind(qc$questions, df)
  define_questions_from_df(qc, df)
}


#' Define an extended question
#'
#' This function allows users to define an extended question, including metadata,
#' feedback and optional image data.
#'
#' Parameter values that are not defined are taken from the category definition,
#' if they are defined there.
#'
#' When defining questions using the extended style via files, there is no limit
#' to the number of fields for answers, feedback for answers, or tags (except for
#' Moodle's limitations when processing imported data). However, when defining
#' questions through this function, the number of fields is limited to the parameters
#' explicitly defined. While it would have been possible to allow a completely
#' variable number of parameters in each section, simplicity was prioritized, and
#' we defined a fixed set of parameters that we believe are more than sufficient
#' for most use cases.
#'
#' In the example provided, we have intentionally used the same structure as in
#' the `define_question()` function to demonstrate that any parameters not needed
#' do not need to be explicitly defined.
#'
#' @param qc A question category object. It should have a `questions` data frame
#'   where new questions will be added.
#' @param category A character string specifying the category of the question.
#' @param type A character string indicating the type of the question.
#' @param fraction A number between 0 and 1.
#' @param id A unique identifier for the question.
#' @param name A character string representing the name of the question.
#' @param author The name of the author of the question.
#' @param fb_general General feedback for the question.
#' @param fb_correct Feedback displayed when the correct answer is selected.
#' @param fb_partially Feedback displayed for partially correct answers.
#' @param fb_incorrect Feedback displayed for incorrect answers.
#' @param question The text of the question.
#' @param image Path to an image file associated with the question.
#' @param image_alt Alternative text describing the image for accessibility.
#'   Required if an image is provided.
#' @param answer The correct answer to the question.
#' @param a_1 Additional possible answer.
#' @param a_2 Additional possible answer.
#' @param a_3 Additional possible answer.
#' @param a_4 Additional possible answer.
#' @param a_5 Additional possible answer.
#' @param a_6 Additional possible answer.
#' @param a_7 Additional possible answer.
#' @param fb_answer Feedback for the correct answer.
#' @param fb_a_1 Feedback for additional answer.
#' @param fb_a_2 Feedback for additional answer.
#' @param fb_a_3 Feedback for additional answer.
#' @param fb_a_4 Feedback for additional answer.
#' @param fb_a_5 Feedback for additional answer.
#' @param fb_a_6 Feedback for additional answer.
#' @param fb_a_7 Feedback for additional answer.
#' @param tag_1 Tag to categorize the question.
#' @param tag_2 Tag to categorize the question.
#' @param tag_3 Tag to categorize the question.
#' @param tag_4 Tag to categorize the question.
#' @param tag_5 Tag to categorize the question.
#' @param tag_6 Tag to categorize the question.
#' @param tag_7 Tag to categorize the question.
#' @param tag_8 Tag to categorize the question.
#' @param tag_9 Tag to categorize the question.
#'
#' @return Returns the updated question category object.
#'
#' @family question definition functions
#'
#' @examples
#'
#' qc <- question_category(category = 'Initial test') |>
#'   define_extended_question(
#'     question = 'What are the basic arithmetic operations?',
#'     answer = 'Addition, subtraction, multiplication and division.',
#'     a_1 = 'Addition and subtraction.',
#'     a_2 = 'Addition, subtraction, multiplication, division and square root.'
#'   )
#'
#' @export
define_extended_question <- function(qc,
                                     category,
                                     type,
                                     fraction,
                                     id,
                                     name,
                                     author,
                                     fb_general,
                                     fb_correct,
                                     fb_partially,
                                     fb_incorrect,
                                     question,
                                     image,
                                     image_alt,
                                     answer,
                                     a_1,
                                     a_2,
                                     a_3,
                                     a_4,
                                     a_5,
                                     a_6,
                                     a_7,
                                     fb_answer,
                                     fb_a_1,
                                     fb_a_2,
                                     fb_a_3,
                                     fb_a_4,
                                     fb_a_5,
                                     fb_a_6,
                                     fb_a_7,
                                     tag_1,
                                     tag_2,
                                     tag_3,
                                     tag_4,
                                     tag_5,
                                     tag_6,
                                     tag_7,
                                     tag_8,
                                     tag_9)
UseMethod("define_extended_question")


#' @rdname define_extended_question
#' @export
define_extended_question.question_category <- function(qc,
                                                       category = '',
                                                       type = '',
                                                       fraction = 0,
                                                       id = '',
                                                       name = '',
                                                       author = '',
                                                       fb_general = '',
                                                       fb_correct = '',
                                                       fb_partially = '',
                                                       fb_incorrect = '',
                                                       question = '',
                                                       image = '',
                                                       image_alt = '',
                                                       answer = '',
                                                       a_1 = '',
                                                       a_2 = '',
                                                       a_3 = '',
                                                       a_4 = '',
                                                       a_5 = '',
                                                       a_6 = '',
                                                       a_7 = '',
                                                       fb_answer = '',
                                                       fb_a_1 = '',
                                                       fb_a_2 = '',
                                                       fb_a_3 = '',
                                                       fb_a_4 = '',
                                                       fb_a_5 = '',
                                                       fb_a_6 = '',
                                                       fb_a_7 = '',
                                                       tag_1 = '',
                                                       tag_2 = '',
                                                       tag_3 = '',
                                                       tag_4 = '',
                                                       tag_5 = '',
                                                       tag_6 = '',
                                                       tag_7 = '',
                                                       tag_8 = '',
                                                       tag_9 = '') {
  df_params <- names(create_common_question_df())

  # Create a named list of argument values
  args <- as.list(environment())
  df_args <- args[df_params]

  # Convert the list to a data frame
  df <- as.data.frame(df_args, stringsAsFactors = FALSE)
  df$fraction <- as.character(fraction)

  df <- rbind(qc$questions, df)
  define_questions_from_df(qc, df)
}

