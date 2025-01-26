test_that("generate_truefalse generates correct XML for 'true' answer", {
  # Inputs
  answer <- "true"
  fb_answer <- "Correct! This statement is true."
  fb_rest <- "Incorrect! The correct answer is true."

  # Expected output
  expected_structure <- structure(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>true</text>\n  <feedback format=\"html\">\n    <text>Correct! This statement is true.</text>\n  </feedback>\n</answer>\n<answer fraction=\"0\" format=\"moodle_auto_format\">\n  <text>false</text>\n  <feedback format=\"html\">\n    <text>Incorrect! The correct answer is true.</text>\n  </feedback>\n</answer>",
    class = c("glue", "character")
  )

  # Run the function
  result <- generate_truefalse(answer, fb_answer, fb_rest, 0)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_truefalse generates correct XML for 'false' answer", {
  # Inputs
  answer <- "false"
  fb_answer <- "Correct! This statement is false."
  fb_rest <- "Incorrect! The correct answer is false."

  # Expected output
  expected_structure <- structure(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">\n  <text>false</text>\n  <feedback format=\"html\">\n    <text>Correct! This statement is false.</text>\n  </feedback>\n</answer>\n<answer fraction=\"0\" format=\"moodle_auto_format\">\n  <text>true</text>\n  <feedback format=\"html\">\n    <text>Incorrect! The correct answer is false.</text>\n  </feedback>\n</answer>",
    class = c("glue", "character")
  )

  # Run the function
  result <- generate_truefalse(answer, fb_answer, fb_rest, 0)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})


