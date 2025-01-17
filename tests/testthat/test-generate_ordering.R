test_that("generate_ordering generates correct XML for a single answer in vertical orientation", {
  # Inputs
  answer <- "Answer 1"
  a_values <- ""
  correct_feedback <- "Well done!"
  incorrect_feedback <- "Try again."
  partially_correct_feedback <- "Almost there."
  orientation <- "v"

  # Expected output
  expected_structure <- paste0(
    "\n<layouttype>VERTICAL</layouttype>",
    "\n<selecttype>ALL</selecttype>",
    "\n<selectcount>0</selectcount>",
    "\n<gradingtype>ABSOLUTE_POSITION</gradingtype>",
    "\n<showgrading>SHOW</showgrading>",
    "\n<numberingstyle>none</numberingstyle>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Well done!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>Almost there.</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Try again.</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect>1</shownumcorrect>",
    "\n<answer fraction=\"1.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 1</text>",
    "\n</answer>",
    "\n<answer fraction=\"2.0000000\" format=\"moodle_auto_format\">",
    "\n  <text></text>",
    "\n</answer>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>"
  )

  # Run the function
  result <- generate_ordering(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback,
    orientation = orientation
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_ordering generates correct XML with multiple answers and horizontal orientation", {
  # Inputs
  answer <- "Answer 1"
  a_values <- c("Answer 2", "Answer 3", "Answer 4")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Not quite."
  partially_correct_feedback <- "You're close."
  orientation <- "h"

  # Expected output
  expected_structure <- paste0(
    "\n<layouttype>HORIZONTAL</layouttype>",
    "\n<selecttype>ALL</selecttype>",
    "\n<selectcount>0</selectcount>",
    "\n<gradingtype>ABSOLUTE_POSITION</gradingtype>",
    "\n<showgrading>SHOW</showgrading>",
    "\n<numberingstyle>none</numberingstyle>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Correct!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>You're close.</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Not quite.</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect>1</shownumcorrect>",
    "\n<answer fraction=\"1.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 1</text>",
    "\n</answer>",
    "\n<answer fraction=\"2.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 2</text>",
    "\n</answer>",
    "\n<answer fraction=\"3.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 3</text>",
    "\n</answer>",
    "\n<answer fraction=\"4.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 4</text>",
    "\n</answer>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>"
  )

  # Run the function
  result <- generate_ordering(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback,
    orientation = orientation
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_ordering handles empty feedback gracefully", {
  # Inputs
  answer <- "Answer 1"
  a_values <- c("Answer 2")
  correct_feedback <- ""
  incorrect_feedback <- ""
  partially_correct_feedback <- ""
  orientation <- "v"

  # Expected output
  expected_structure <- paste0(
    "\n<layouttype>VERTICAL</layouttype>",
    "\n<selecttype>ALL</selecttype>",
    "\n<selectcount>0</selectcount>",
    "\n<gradingtype>ABSOLUTE_POSITION</gradingtype>",
    "\n<showgrading>SHOW</showgrading>",
    "\n<numberingstyle>none</numberingstyle>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text></text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text></text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text></text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect>1</shownumcorrect>",
    "\n<answer fraction=\"1.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 1</text>","\n</answer>",
    "\n<answer fraction=\"2.0000000\" format=\"moodle_auto_format\">",
    "\n  <text>Answer 2</text>",
    "\n</answer>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>",
    "\n    <hint format=\"html\">",
    "\n      <text></text>",
    "\n    </hint>"
  )

  # Run the function
  result <- generate_ordering(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback,
    orientation = orientation
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
