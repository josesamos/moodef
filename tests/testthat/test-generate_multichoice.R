test_that("generate_multichoice generates correct XML for single correct answer", {
  # Inputs
  answer <- "Correct Answer"
  rest <- c("Wrong Answer 1", "Wrong Answer 2")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  fb_partially <- "Almost there!"

  # Expected output
  n <- length(rest)
  value <- sprintf("-%2.15f", 100 / n)
  expected_structure <- paste0(
    "\n<single>true</single>",
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<answernumbering>abc</answernumbering>",
    "\n<showstandardinstruction>0</showstandardinstruction>",
    "\n<correctfeedback format=\"moodle_auto_format\"> <text>Correct!</text> </correctfeedback>",
    "\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text>Almost there!</text> </partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"moodle_auto_format\"> <text>Incorrect!</text> </incorrectfeedback>",
    "\n<answer fraction=\"100\" format=\"html\">",
    "\n   <text>Correct Answer</text>",
    "\n   <feedback format=\"html\"> <text>Correct!</text> </feedback>",
    "\n</answer>",
    "\n<answer fraction=\"-50.000000000000000\" format=\"html\">",
    "\n   <text>Wrong Answer 1</text>",
    "\n   <feedback format=\"html\"> <text>Incorrect!</text> </feedback>",
    "\n</answer>",
    "\n<answer fraction=\"-50.000000000000000\" format=\"html\">",
    "\n   <text>Wrong Answer 2</text>",
    "\n   <feedback format=\"html\"> <text>Incorrect!</text> </feedback>",
    "\n</answer>"
  )

  # Run the function
  result <- generate_multichoice(
    answer = answer,
    rest = rest,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_multichoice handles custom feedback for rest answers", {
  # Inputs
  answer <- "Correct Answer"
  rest <- c("Wrong Answer 1", "Wrong Answer 2")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  fb_rest <- c("Custom Feedback 1", "Custom Feedback 2")

  # Expected output
  n <- length(rest)
  value <- sprintf("-%2.15f", 100 / n)
  expected_structure <- paste0(
    "\n<single>true</single>",
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<answernumbering>abc</answernumbering>",
    "\n<showstandardinstruction>0</showstandardinstruction>",
    "\n<correctfeedback format=\"moodle_auto_format\"> <text>Correct!</text> </correctfeedback>",
    "\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text></text> </partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"moodle_auto_format\"> <text>Incorrect!</text> </incorrectfeedback>",
    "\n<answer fraction=\"100\" format=\"html\">",
    "\n   <text>Correct Answer</text>",
    "\n   <feedback format=\"html\"> <text>Correct!</text> </feedback>",
    "\n</answer>","\n<answer fraction=\"-50.000000000000000\" format=\"html\">",
    "\n   <text>Wrong Answer 1</text>",
    "\n   <feedback format=\"html\"> <text>Custom Feedback 1</text> </feedback>",
    "\n</answer>",
    "\n<answer fraction=\"-50.000000000000000\" format=\"html\">",
    "\n   <text>Wrong Answer 2</text>",
    "\n   <feedback format=\"html\"> <text>Custom Feedback 2</text> </feedback>",
    "\n</answer>"
  )

  # Run the function
  result <- generate_multichoice(
    answer = answer,
    rest = rest,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_rest = fb_rest
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_multichoice handles empty inputs gracefully", {
  # Inputs
  answer <- ""
  rest <- character(0)
  correct_feedback <- ""
  incorrect_feedback <- ""
  fb_partially <- ""

  # Expected output
  expected_structure <- paste0(
    "\n<single>true</single>",
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<answernumbering>abc</answernumbering>",
    "\n<showstandardinstruction>0</showstandardinstruction>",
    "\n<correctfeedback format=\"moodle_auto_format\"> <text></text> </correctfeedback>",
    "\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text></text> </partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"moodle_auto_format\"> <text></text> </incorrectfeedback>",
    "\n<answer fraction=\"100\" format=\"html\">",
    "\n   <text></text>",
    "\n   <feedback format=\"html\"> <text></text> </feedback>",
    "\n</answer>"
  )

  # Run the function
  result <- generate_multichoice(
    answer = answer,
    rest = rest,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
