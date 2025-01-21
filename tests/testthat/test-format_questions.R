test_that("filter_non_empty_answers removes empty answers and trims whitespace", {
  # Test with mixed empty and non-empty strings
  result <- filter_non_empty_answers(" answer1 ", "", "   ", "answer2")
  expect_equal(result, c("answer1", "answer2"))

  # Test with only empty strings
  result <- filter_non_empty_answers("", "   ", "")
  expect_equal(result, NULL)

  # Test with only non-empty strings
  result <- filter_non_empty_answers("answer1", "answer2", " answer3 ")
  expect_equal(result, c("answer1", "answer2", "answer3"))

  # Test with no inputs
  result <- filter_non_empty_answers()
  expect_equal(result, NULL)

  # Test with numeric inputs (converted to character implicitly)
  result <- filter_non_empty_answers(1, 0, 3.14, "")
  expect_equal(result, c("1", "0", "3.14"))
})


test_that("generate_question_body works correctly for each question type", {
  # Numerical
  mock_answer <- "42"
  mock_rest <- list()
  result <- generate_question_body("numerical", mock_answer, mock_rest, '', '', '', '', '', NULL, '', '')
  expect_equal(result, generate_numerical(mock_answer, mock_rest, '', NULL))

  # Multichoice
  mock_answer <- "A"
  mock_rest <- list("Option B", "Option C")
  result <- generate_question_body("multichoice", mock_answer, mock_rest, "Correct!", "Incorrect!", '', '', '', NULL, '', '', 1)
  expect_equal(result, generate_multichoice(mock_answer, mock_rest, "Correct!", "Incorrect!", '', '', NULL, 1))

  # Ordering
  mock_answer <- c("Step 1", "Step 2", "Step 3")
  result <- generate_question_body("ordering", mock_answer, mock_rest, "Good job!", "Try again!", "Almost!", "vertical")
  expect_equal(result, generate_ordering(mock_answer, mock_rest, "Good job!", "Try again!", "Almost!", "vertical"))

  # DDWTOS
  mock_answer <- "Drag this <gap> to here."
  result <- generate_question_body("ddwtos", mock_answer, mock_rest, "Perfect!", "Not quite.", "Close!", NULL)
  expect_equal(result, generate_ddwtos(mock_answer, mock_rest, "Perfect!", "Not quite.", "Close!"))

  # Gapselect
  result <- generate_question_body("gapselect", mock_answer, mock_rest, "Correct!", "Wrong.", "Nearly!", NULL)
  expect_equal(result, generate_gapselect(mock_answer, mock_rest, "Correct!", "Wrong.", "Nearly!"))

  # Matching
  mock_answer <- c("A" = "1", "B" = "2")
  result <- generate_question_body("matching", mock_answer, mock_rest, "Matched!", "Mismatch!", "Partial match!", NULL)
  expect_equal(result, generate_matching(mock_answer, mock_rest, "Matched!", "Mismatch!", "Partial match!"))

  # Essay
  result <- generate_question_body("essay", NULL, NULL, NULL, NULL, NULL, NULL)
  expect_equal(result, generate_essay())

  # True/False
  mock_answer <- "true"
  result <- generate_question_body("truefalse", mock_answer, NULL, NULL, NULL, NULL, NULL, '', '', NULL, NULL, 1)
  expect_equal(result, generate_truefalse(mock_answer, '', NULL, 1))

  # Shortanswer
  mock_answer <- "Answer"
  result <- generate_question_body("shortanswer", mock_answer, NULL, NULL, NULL, NULL, NULL, '', '', NULL, NULL, NULL)
  expect_equal(result, generate_shortanswer(mock_answer, ''))

})

test_that("extract_type_orientation works with type and orientation", {
  result <- extract_type_orientation("ordering<|>h")
  expect_equal(result$type, "ordering")
  expect_equal(result$orientation, "h")
})

test_that("extract_type_orientation works with type only", {
  result <- extract_type_orientation("ordering")
  expect_equal(result$type, "ordering")
  expect_equal(result$orientation, "v") # Default orientation
})

test_that("get_vector_answer works correctly", {

  # Test when answer is a single value
  expect_equal(get_vector_answer("A"), c("A"),
               info = "Should return a vector with one element for a single value")

  # Test when answer is an empty string
  expect_equal(get_vector_answer(""), c(""),
               info = "Should return a vector with an empty string for an empty input string")
})


test_that("generate_question_name generates correct XML with valid inputs", {
  # Input
  first_question_number <- 1
  type <- "multichoice"
  question <- "What is the capital of France?"

  # Expected name
  expected_name <- "q001_multichoice_horizontal_what_is_the_capital_of_france"

  # Call the function
  result <- generate_question_name(first_question_number, type, question)

  # Expectations
  expect_match(
    result,
    structure(
      "q_001_multichoice_what_is_the_capital_of_france",
      class = c("glue", "character")
    ),
    fixed = TRUE
  )
})

test_that("generate_question_name truncates question to 40 characters", {
  # Input
  first_question_number <- 15
  type <- "shortanswer"
  question <- "This is a very long question that exceeds forty characters in length."

  # Expected name
  expected_name <- "q015_shortanswer_this_is_a_very_long_question"

  # Call the function
  result <- generate_question_name(first_question_number, type, question)

  # Expectations
  expect_match(
    result,
    structure(
      "q_015_shortanswer_this_is_a_very_long_question_that_exceed",
      class = c("glue", "character")
    ),
    fixed = TRUE
  )
})
