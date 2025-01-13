test_that("question_category", {
  qc <- question_category(category = 'Initial test') |>
    define_question(
      question = 'What are the basic arithmetic operations?',
      answer = 'Addition, subtraction, multiplication and division.',
      w_1 = 'Addition and subtraction.',
      w_2 = 'Addition, subtraction, multiplication, division and square root.'
    )

  qc2 <- question_category(category = 'Initial test') |>
    define_question(
      question = 'What are the basic arithmetic operations?',
      answer = 'Addition, subtraction, multiplication and division.',
      w_1 = 'Addition and subtraction.',
      w_2 = 'Addition, subtraction, multiplication, division and square root.',
      w_3 = 'Addition, subtraction, multiplication, division and square root 2.',
      w_4 = 'Addition, subtraction, multiplication, division and square root 3.'
    )


  expect_equal(qc,
               structure(
                 list(
                   category = "Initial test",
                   first_question_number = 2,
                   copyright = "",
                   license = "",
                   author = "",
                   correct_feedback = "Correct.",
                   partially_correct_feedback = "Partially correct.",
                   incorrect_feedback = "Incorrect.",
                   adapt_images = FALSE,
                   width = 800,
                   height = 600,
                   a_n = 3,
                   questions = structure(
                     list(
                       first_question_number = 1,
                       copyright = "",
                       license = "",
                       correct_feedback = "Correct.",
                       partially_correct_feedback = "Partially correct.",
                       incorrect_feedback = "Incorrect.",
                       adapt_images = FALSE,
                       width = 800,
                       height = 600,
                       type = '',
                       question = "What are the basic arithmetic operations?",
                       image = "",
                       image_alt = "",
                       answer = "Addition, subtraction, multiplication and division.",
                       a_1 = "Addition and subtraction.",
                       a_2 = "Addition, subtraction, multiplication, division and square root.",
                       a_3 = ""
                     ),
                     row.names = c(NA,-1L),
                     class = "data.frame"
                   )
                 ),
                 class = "question_category"
               ))

  expect_equal(qc2,
               structure(
                 list(
                   category = "Initial test",
                   first_question_number = 2,
                   copyright = "",
                   license = "",
                   author = "",
                   correct_feedback = "Correct.",
                   partially_correct_feedback = "Partially correct.",
                   incorrect_feedback = "Incorrect.",
                   adapt_images = FALSE,
                   width = 800,
                   height = 600,
                   a_n = 4L,
                   questions = structure(
                     list(
                       first_question_number = 1,
                       copyright = "",
                       license = "",
                       correct_feedback = "Correct.",
                       partially_correct_feedback = "Partially correct.",
                       incorrect_feedback = "Incorrect.",
                       adapt_images = FALSE,
                       width = 800,
                       height = 600,
                       type = '',
                       question = "What are the basic arithmetic operations?",
                       image = "",
                       image_alt = "",
                       answer = "Addition, subtraction, multiplication and division.",
                       a_1 = "Addition and subtraction.",
                       a_2 = "Addition, subtraction, multiplication, division and square root.",
                       a_3 = "Addition, subtraction, multiplication, division and square root 2.",
                       a_4 = "Addition, subtraction, multiplication, division and square root 3."
                     ),
                     row.names = c(NA,-1L),
                     class = "data.frame"
                   )
                 ),
                 class = "question_category"
               ))
})
