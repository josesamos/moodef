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

  expect_equal(qc, structure(
    list(
      category = "Initial test",
      penalty = "0",
      first_question_number = 12,
      copyright = "",
      license = "",
      author = "",
      correct_feedback = "Correct.",
      partially_correct_feedback = "Partially correct.",
      incorrect_feedback = "Incorrect.",
      adapt_images = TRUE,
      width = 800,
      height = 600,
      questions = structure(
        list(
          category = c(
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test",
            "Initial test"
          ),
          type = c(
            "multichoice",
            "matching",
            "truefalse",
            "shortanswer",
            "ddwtos",
            "gapselect",
            "ordering<|>h",
            "ordering<|>v",
            "numerical",
            "numerical",
            "essay"
          ),
          penalty = c("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"),
          id = c("", "", "", "", "", "", "", "", "", "", ""),
          name = c(
            "q_001_multichoice_what_are_the_basic_arithmetic_operations",
            "q_002_matching_match_each_operation_with_its_symbol",
            "q_003_truefalse_the_square_root_is_a_basic_arithmetic_op",
            "q_004_shortanswer_what_basic_operation_does_it_have_as_a",
            "q_005_ddwtos_the_symbol_for_addition_is_1_the_sy",
            "q_006_gapselect_the_symbol_for_addition_is_1_the_sy",
            "q_007_ordering_h_sort_the_result_from_smallest_to_largest",
            "q_008_ordering_v_sort_the_result_from_smallest_to_largest",
            "q_009_numerical_what_is_the_result_of_sqrt_4",
            "q_010_numerical_what_is_the_result_of_4_3",
            "q_011_essay_describe_the_addition_operation"
          ),
          author = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_general = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_correct = c(
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
          fb_partially = c(
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
          fb_incorrect = c(
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
          question = c(
            "What are the basic arithmetic operations?",
            "Match each operation with its symbol.",
            "The square root is a basic arithmetic operation.",
            "What basic operation does it have as a + symbol?",
            "The symbol for addition is [[1]], the symbol for subtraction is [[2]].",
            "The symbol for addition is [[1]], the symbol for subtraction is [[2]].",
            "Sort the result from smallest to largest.",
            "Sort the result from smallest to largest.",
            "What is the result of SQRT(4)?",
            "What is the result of 4/3?",
            "Describe the addition operation."
          ),
          image = c("", "", "", "", "", "", "", "", "", "", ""),
          image_alt = c("", "", "", "", "", "", "", "", "", "", ""),
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
          a_3 = c("", "", "", "", "", "", "6*2", "6*2", "", "", ""),
          a_4 = c("", "", "", "", "", "", "", "", "", "", ""),
          a_5 = c("", "", "", "", "", "", "", "", "", "", ""),
          a_6 = c("", "", "", "", "", "", "", "", "", "", ""),
          a_7 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_answer = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_1 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_2 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_3 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_4 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_5 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_6 = c("", "", "", "", "", "", "", "", "", "", ""),
          fb_a_7 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_1 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_2 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_3 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_4 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_5 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_6 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_7 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_8 = c("", "", "", "", "", "", "", "", "", "", ""),
          tag_9 = c("", "", "", "", "", "", "", "", "", "", "")
        ),
        row.names = c(NA, -11L),
        class = "data.frame"
      )
    ),
    class = "question_category"
  ))
})


