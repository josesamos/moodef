test_that("generate_multichoice generates correct XML for single correct answer", {
  # Inputs
  answer <- "Correct Answer"
  a_values <- c("Wrong Answer 1", "Wrong Answer 2")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  fb_partially <- "Almost there!"

  # Expected output
  n <- length(a_values)
  value <- sprintf("-%2.15f", 100 / n)
  expected_structure <- paste0(
    "\n<single>true</single>",
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<answernumbering>none</answernumbering>",
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
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially,
    fb_answer = '',
    fb_a_values = NULL,
    fraction = 1
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_multichoice handles custom feedback for a_values answers", {
  # Inputs
  answer <- "Correct Answer"
  a_values <- c("Wrong Answer 1", "Wrong Answer 2")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  fb_partially <- "Almost there!"
  fb_a_values <- c("Custom Feedback 1", "Custom Feedback 2")

  # Expected output
  n <- length(a_values)
  value <- sprintf("-%2.15f", 100 / n)
  expected_structure <- paste0(
    "\n<single>true</single>",
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<answernumbering>none</answernumbering>",
    "\n<showstandardinstruction>0</showstandardinstruction>",
    "\n<correctfeedback format=\"moodle_auto_format\"> <text>Correct!</text> </correctfeedback>",
    "\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text>Almost there!</text> </partiallycorrectfeedback>",
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
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially,
    fb_a_values = fb_a_values,
    fb_answer = '',
    fraction = 1
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})



