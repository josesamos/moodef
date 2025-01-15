test_that("generate_shortanswer generates correct XML for answer with feedback", {
  # Inputs
  answer <- "Correct Answer"
  fb_answer <- "Great job!"

  # Expected output
  expected_structure <- structure(
    "\n<usecase>0</usecase>\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>Correct Answer</text>\n  <feedback format=\"html\">\n    <text>Great job!</text>\n  </feedback>\n</answer>",
    class = c("glue", "character"))

  # Run the function
  result <- generate_shortanswer(answer, fb_answer)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_shortanswer generates correct XML for answer without feedback", {
  # Inputs
  answer <- "Correct Answer"
  fb_answer <- ""

  # Expected output
  expected_structure <- structure(
    "\n<usecase>0</usecase>\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>Correct Answer</text>\n  <feedback format=\"html\">\n    <text></text>\n  </feedback>\n</answer>",
    class = c("glue", "character"))

  # Run the function
  result <- generate_shortanswer(answer, fb_answer)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_shortanswer handles special characters in answer and feedback", {
  # Inputs with special characters
  answer <- "Correct <Answer> & Special 'Characters'"
  fb_answer <- "Good job! Avoid using \"double quotes\"."

  # Expected output
  expected_structure <- structure(
    "\n<usecase>0</usecase>\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>Correct <Answer> & Special 'Characters'</text>\n  <feedback format=\"html\">\n    <text>Good job! Avoid using \"double quotes\".</text>\n  </feedback>\n</answer>",
    class = c("glue", "character"))

  # Run the function
  result <- generate_shortanswer(answer, fb_answer)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_shortanswer handles numeric answers correctly", {
  # Inputs with numeric answer
  answer <- "42"
  fb_answer <- "That's the correct number!"

  # Expected output
  expected_structure <- structure(
    "\n<usecase>0</usecase>\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>42</text>\n  <feedback format=\"html\">\n    <text>That's the correct number!</text>\n  </feedback>\n</answer>",
    class = c("glue",  "character"))

  # Run the function
  result <- generate_shortanswer(answer, fb_answer)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
