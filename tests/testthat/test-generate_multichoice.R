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
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially,
    fb_answer = '', fb_a_values = NULL
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
    "\n<answernumbering>abc</answernumbering>",
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
    fb_answer = ''
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})


test_that("generate_multichoice generates correct XML when fb_answer is not empty", {
  answer <- "Correct Answer"
  a_values <- c("Wrong Answer 1", "Wrong Answer 2", "Wrong Answer 3")
  correct_feedback <- "Good job!"
  incorrect_feedback <- "That's not correct."
  fb_partially <- "Partially correct."
  fb_answer <- "Specific feedback for the correct answer."
  fb_a_values <- c("Feedback for Wrong Answer 1", "Feedback for Wrong Answer 2", "Feedback for Wrong Answer 3")

  result <- generate_multichoice(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    fb_partially = fb_partially,
    fb_answer = fb_answer,
    fb_a_values = fb_a_values
  )

  # Check correct answer and its feedback
  expect_match(result, '<answer fraction="100" format="html">', fixed = TRUE)
  expect_match(result, '<text>Correct Answer</text>', fixed = TRUE)
  expect_match(result, '<text>Specific feedback for the correct answer.</text>', fixed = TRUE)

  # Check incorrect answers and their feedback
  for (i in seq_along(a_values)) {
    expect_match(result, sprintf('<text>%s</text>', a_values[i]), fixed = TRUE)
    expect_match(result, sprintf('<text>%s</text>', fb_a_values[i]), fixed = TRUE)
  }

  # Verify feedback for partially correct
  expect_match(result, '<partiallycorrectfeedback format="moodle_auto_format">', fixed = TRUE)
  expect_match(result, '<text>Partially correct.</text>', fixed = TRUE)

  # Verify feedback for incorrect answers
  expect_match(result, '<incorrectfeedback format="moodle_auto_format">', fixed = TRUE)
  expect_match(result, '<text>That\'s not correct.</text>', fixed = TRUE)
})

