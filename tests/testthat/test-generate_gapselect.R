test_that("generate_gapselect generates correct XML structure with a single answer", {
  # Inputs
  answer <- "Correct Answer"
  a_values <- character(0)  # No additional options
  correct_feedback <- "Well done!"
  incorrect_feedback <- "Try again!"
  partially_correct_feedback <- "Almost there!"

  # Expected structure
  expected_structure <- paste0(
    "\n<shuffleanswers>1</shuffleanswers>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Well done!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>Almost there!</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Try again!</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect/>",
    "\n<selectoption>",
    "\n  <text>Correct Answer</text>",
    "\n  <group>1</group>",
    "\n</selectoption>"
  )

  # Run the function
  result <- generate_gapselect(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_gapselect generates correct XML structure with multiple options", {
  # Inputs
  answer <- "Correct Answer"
  a_values <- c("Option 1", "Option 2", "Option 3")
  correct_feedback <- "Well done!"
  incorrect_feedback <- "Try again!"
  partially_correct_feedback <- "Almost there!"

  # Expected structure
  expected_structure <- paste0(
    "\n<shuffleanswers>1</shuffleanswers>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Well done!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>Almost there!</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Try again!</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect/>",
    "\n<selectoption>",
    "\n  <text>Correct Answer</text>",
    "\n  <group>1</group>",
    "\n</selectoption>",
    "\n<selectoption>",
    "\n  <text>Option 1</text>",
    "\n  <group>1</group>",
    "\n</selectoption>",
    "\n<selectoption>",
    "\n  <text>Option 2</text>",
    "\n  <group>1</group>",
    "\n</selectoption>",
    "\n<selectoption>",
    "\n  <text>Option 3</text>",
    "\n  <group>1</group>",
    "\n</selectoption>"
  )

  # Run the function
  result <- generate_gapselect(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

