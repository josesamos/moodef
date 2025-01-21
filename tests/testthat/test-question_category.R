test_that("define_question", {
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


  expect_equal(qc, structure(
    list(
      category = "Initial test",
      penalty = 0,
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
      questions = structure(
        list(
          category = "Initial test",
          type = "multichoice",
          penalty = "",
          id = "",
          name = "q_001_multichoice_what_are_the_basic_arithmetic_operations",
          author = "",
          fb_general = "",
          fb_correct = "Correct.",
          fb_partially = "Partially correct.",
          fb_incorrect = "Incorrect.",
          question = "What are the basic arithmetic operations?",
          image = "",
          image_alt = "",
          answer = "Addition, subtraction, multiplication and division.",
          a_1 = "Addition and subtraction.",
          a_2 = "Addition, subtraction, multiplication, division and square root.",
          a_3 = "",
          a_4 = "",
          a_5 = "",
          a_6 = "",
          a_7 = "",
          fb_answer = "",
          fb_a_1 = "",
          fb_a_2 = "",
          fb_a_3 = "",
          fb_a_4 = "",
          fb_a_5 = "",
          fb_a_6 = "",
          fb_a_7 = "",
          tag_1 = "",
          tag_2 = "",
          tag_3 = "",
          tag_4 = "",
          tag_5 = "",
          tag_6 = "",
          tag_7 = "",
          tag_8 = "",
          tag_9 = ""
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    ),
    class = "question_category"
  ))

  expect_equal(qc2, structure(
    list(
      category = "Initial test",
      penalty = 0,
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
      questions = structure(
        list(
          category = "Initial test",
          type = "multichoice",
          penalty = "",
          id = "",
          name = "q_001_multichoice_what_are_the_basic_arithmetic_operations",
          author = "",
          fb_general = "",
          fb_correct = "Correct.",
          fb_partially = "Partially correct.",
          fb_incorrect = "Incorrect.",
          question = "What are the basic arithmetic operations?",
          image = "",
          image_alt = "",
          answer = "Addition, subtraction, multiplication and division.",
          a_1 = "Addition and subtraction.",
          a_2 = "Addition, subtraction, multiplication, division and square root.",
          a_3 = "Addition, subtraction, multiplication, division and square root 2.",
          a_4 = "Addition, subtraction, multiplication, division and square root 3.",
          a_5 = "",
          a_6 = "",
          a_7 = "",
          fb_answer = "",
          fb_a_1 = "",
          fb_a_2 = "",
          fb_a_3 = "",
          fb_a_4 = "",
          fb_a_5 = "",
          fb_a_6 = "",
          fb_a_7 = "",
          tag_1 = "",
          tag_2 = "",
          tag_3 = "",
          tag_4 = "",
          tag_5 = "",
          tag_6 = "",
          tag_7 = "",
          tag_8 = "",
          tag_9 = ""
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    ),
    class = "question_category"
  ))
})



test_that("define_extended_question", {
  qc <- question_category(category = 'Initial test') |>
    define_extended_question(
      question = 'What are the basic arithmetic operations?',
      answer = 'Addition, subtraction, multiplication and division.',
      a_1 = 'Addition and subtraction.',
      a_2 = 'Addition, subtraction, multiplication, division and square root.'
    )

  qc2 <- question_category(category = 'Initial test') |>
    define_extended_question(
      question = 'What are the basic arithmetic operations?',
      answer = 'Addition, subtraction, multiplication and division.',
      a_1 = 'Addition and subtraction.',
      a_2 = 'Addition, subtraction, multiplication, division and square root.',
      a_3 = 'Addition, subtraction, multiplication, division and square root 2.',
      a_4 = 'Addition, subtraction, multiplication, division and square root 3.'
    )


  expect_equal(qc, structure(
    list(
      category = "Initial test",
      penalty = 0,
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
      questions = structure(
        list(
          category = "Initial test",
          type = "multichoice",
          penalty = 0,
          id = "",
          name = "q_001_multichoice_what_are_the_basic_arithmetic_operations",
          author = "",
          fb_general = "",
          fb_correct = "Correct.",
          fb_partially = "Partially correct.",
          fb_incorrect = "Incorrect.",
          question = "What are the basic arithmetic operations?",
          image = "",
          image_alt = "",
          answer = "Addition, subtraction, multiplication and division.",
          a_1 = "Addition and subtraction.",
          a_2 = "Addition, subtraction, multiplication, division and square root.",
          a_3 = "",
          a_4 = "",
          a_5 = "",
          a_6 = "",
          a_7 = "",
          fb_answer = "",
          fb_a_1 = "",
          fb_a_2 = "",
          fb_a_3 = "",
          fb_a_4 = "",
          fb_a_5 = "",
          fb_a_6 = "",
          fb_a_7 = "",
          tag_1 = "",
          tag_2 = "",
          tag_3 = "",
          tag_4 = "",
          tag_5 = "",
          tag_6 = "",
          tag_7 = "",
          tag_8 = "",
          tag_9 = ""
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    ),
    class = "question_category"
  ))

  expect_equal(qc2, structure(
    list(
      category = "Initial test",
      penalty = 0,
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
      questions = structure(
        list(
          category = "Initial test",
          type = "multichoice",
          penalty = 0,
          id = "",
          name = "q_001_multichoice_what_are_the_basic_arithmetic_operations",
          author = "",
          fb_general = "",
          fb_correct = "Correct.",
          fb_partially = "Partially correct.",
          fb_incorrect = "Incorrect.",
          question = "What are the basic arithmetic operations?",
          image = "",
          image_alt = "",
          answer = "Addition, subtraction, multiplication and division.",
          a_1 = "Addition and subtraction.",
          a_2 = "Addition, subtraction, multiplication, division and square root.",
          a_3 = "Addition, subtraction, multiplication, division and square root 2.",
          a_4 = "Addition, subtraction, multiplication, division and square root 3.",
          a_5 = "",
          a_6 = "",
          a_7 = "",
          fb_answer = "",
          fb_a_1 = "",
          fb_a_2 = "",
          fb_a_3 = "",
          fb_a_4 = "",
          fb_a_5 = "",
          fb_a_6 = "",
          fb_a_7 = "",
          tag_1 = "",
          tag_2 = "",
          tag_3 = "",
          tag_4 = "",
          tag_5 = "",
          tag_6 = "",
          tag_7 = "",
          tag_8 = "",
          tag_9 = ""
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    ),
    class = "question_category"
  ))
})
