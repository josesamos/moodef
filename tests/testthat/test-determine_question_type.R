test_that("determine_question_type correctly identifies question types", {
  # Numerical questions
  result <- determine_question_type("", "What is 5+3?", "8", list())
  expect_equal(result, "numerical")

  # ddmarker questions
  result <- determine_question_type("", "Position", c("circle", "377,157;15", "Toise"), list())
  expect_equal(result, "ddmarker")

  # Single answer, no gaps, type unspecified -> multichoice
  result <- determine_question_type("", "Choose the correct answer", "A", "B")
  expect_equal(result, "multichoice")

  # Single answer, no gaps, type specified -> ordering
  result <- determine_question_type("x", "Arrange these in order", "A", "B")
  expect_equal(result, "ordering<|>v")

  # Single answer, with gaps, type unspecified -> ddwtos
  result <- determine_question_type("", "Fill [[1]] in the [[2]]", "answer", "B")
  expect_equal(result, "ddwtos")

  # Single answer, with gaps, type specified -> gapselect
  result <- determine_question_type("x", "Fill [[1]] in the [[2]]", "answer", "B")
  expect_equal(result, "gapselect")

  # Multiple answers -> matching
  result <- determine_question_type("", "Match the following", c("A", "B"), "B")
  expect_equal(result, "matching")

  # Empty answer -> essay
  result <- determine_question_type("", "Write an essay on...", "", NULL)
  expect_equal(result, "essay")

  # Boolean answers -> truefalse
  result <- determine_question_type("", "Is the earth round?", "true", NULL)
  expect_equal(result, "truefalse")

  result <- determine_question_type("", "Is the earth flat?", "FALSE", NULL)
  expect_equal(result, "truefalse")

  # Short answer
  result <- determine_question_type("", "What is the capital of France?", "Paris", NULL)
  expect_equal(result, "shortanswer")
})

