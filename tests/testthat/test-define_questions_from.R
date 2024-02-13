test_that("define_questions_from", {
  file <- system.file("extdata", "questions.csv", package = "moodef")
  df <- read_question_csv(file = file)

  qc <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_data_frame(df)

  qc2 <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_csv(file = file)

  file3 <- system.file("extdata", "questions.xlsx", package = "moodef")
  df3 <- read_question_excel(file = file3)
  names(df3) <- names(df)

  df4 <- read_question_excel(file = file3, sheet_name = 'Hoja1')
  names(df4) <- names(df)

  df5 <- read_question_excel(file = file3, sheet_index = 1)
  names(df5) <- names(df)

  qc3 <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_excel(file = file3)



  expect_equal(df, df3)

  expect_equal(df, df4)

  expect_equal(df, df5)

  expect_equal(qc, qc2)

  expect_equal(qc, qc3)

  expect_equal(qc,
               structure(
                 list(
                   category = "Initial test",
                   first_question_number = 12,
                   copyright = "",
                   license = "",
                   correct_feedback = "Correct.",
                   partially_correct_feedback = "Partially correct.",
                   incorrect_feedback = "Incorrect.",
                   adapt_images = TRUE,
                   width = 800,
                   height = 600,
                   a_n = 3,
                   questions = structure(
                     list(
                       first_question_number = c(1, 2,
                                                 3, 4, 5, 6, 7, 8, 9, 10, 11),
                       copyright = c("", "", "", "",
                                     "", "", "", "", "", "", ""),
                       license = c("", "", "", "",
                                   "", "", "", "", "", "", ""),
                       correct_feedback = c(
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct.",
                         "Correct."
                       ),
                       partially_correct_feedback = c(
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct.",
                         "Partially correct."
                       ),
                       incorrect_feedback = c(
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect.",
                         "Incorrect."
                       ),
                       adapt_images = c(TRUE,
                                        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                       width = c(800, 800, 800, 800, 800, 800, 800, 800, 800,
                                 800, 800),
                       height = c(600, 600, 600, 600, 600, 600, 600,
                                  600, 600, 600, 600),
                       type = c("", "", "", "", "", "x", "h",
                                "x", "", "", ""),
                       question = c(
                         "What are the basic arithmetic operations?",
                         "Match each operation with its symbol.",
                         "The square root is a basic arithmetic operation.",
                         "What basic operation does it have as a \"+\" symbol?",
                         "The symbol for addition is [[1]], the symbol for subtraction is [[2]].",
                         "The symbol for addition is [[1]], the symbol for subtraction is [[2]].",
                         "Sort the result from smallest to largest.",
                         "Sort the result from smallest to largest.",
                         "What is the result of SQRT(4)?",
                         "What is the result of 4/3?",
                         "Describe the addition operation."
                       ),
                       image = c("", "", "",
                                 "", "", "", "", "", "", "", ""),
                       image_alt = c("", "", "",
                                     "", "", "", "", "", "", "", ""),
                       answer = c(
                         "Addition, subtraction, multiplication and division.",
                         "Addition<|>+",
                         "False",
                         "Addition",
                         "+",
                         "+",
                         "6/2",
                         "6/2",
                         "2",
                         "1.33<|>0.03",
                         ""
                       ),
                       a_1 = c(
                         "Addition and subtraction.",
                         "Subtraction<|>-",
                         "",
                         "",
                         "-",
                         "-",
                         "6-2",
                         "6-2",
                         "-2",
                         "",
                         ""
                       ),
                       a_2 = c(
                         "Addition, subtraction, multiplication, division and square root.",
                         "Multiplication<|>*",
                         "",
                         "",
                         "",
                         "",
                         "6+2",
                         "6+2",
                         "",
                         "",
                         ""
                       ),
                       a_3 = c("", "", "", "", "", "", "6*2", "6*2", "", "",
                               "")
                     ),
                     row.names = c(NA,-11L),
                     class = "data.frame"
                   )
                 ),
                 class = "question_category"
               ))
})
