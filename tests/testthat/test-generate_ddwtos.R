test_that("generate_ddwtos produces correct XML structure", {
  # Inputs
  answer <- "Correct Answer"
  a_values <- c("Distractor 1", "Distractor 2")
  correct_feedback <- "Well done!"
  incorrect_feedback <- "Try again."
  partially_correct_feedback <- "Almost there."

  # Expected structure
  expected_structure <- paste0(
    "\n<shuffleanswers>1</shuffleanswers>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>", correct_feedback, "</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>", partially_correct_feedback, "</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>", incorrect_feedback, "</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect/>",
    "\n<dragbox>",
    "\n  <text>", answer, "</text>",
    "\n  <group>1</group>",
    "\n</dragbox>",
    "\n<dragbox>",
    "\n  <text>", a_values[1], "</text>",
    "\n  <group>1</group>",
    "\n</dragbox>",
    "\n<dragbox>",
    "\n  <text>", a_values[2], "</text>",
    "\n  <group>1</group>",
    "\n</dragbox>"
  )

  # Run the function
  result <- generate_ddwtos(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

