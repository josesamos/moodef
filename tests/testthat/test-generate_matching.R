test_that("generate_matching generates correct XML structure for a single pair", {
  # Inputs
  answer <- c("Question 1", "Answer 1")
  a_values <- character(0)  # No additional pairs
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  partially_correct_feedback <- "Partially correct!"

  # Expected structure
  expected_structure <- paste0(
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Correct!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>Partially correct!</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Incorrect!</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect/>",
    "\n<subquestion format=\"html\">",
    "\n  <text><![CDATA[<p>Question 1<br></p>]]></text>",
    "\n  <answer>",
    "\n    <text>Answer 1</text>",
    "\n  </answer>",
    "\n</subquestion>"
  )

  # Run the function
  result <- generate_matching(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_matching generates correct XML structure for multiple pairs", {
  # Inputs
  answer <- c("Question 1", "Answer 1")
  a_values <- c("Question 2|Answer 2", "Question 3|Answer 3")
  correct_feedback <- "Correct!"
  incorrect_feedback <- "Incorrect!"
  partially_correct_feedback <- "Partially correct!"

  # Expected structure
  expected_structure <- paste0(
    "\n<shuffleanswers>true</shuffleanswers>",
    "\n<correctfeedback format=\"html\">",
    "\n  <text>Correct!</text>",
    "\n</correctfeedback>",
    "\n<partiallycorrectfeedback format=\"html\">",
    "\n  <text>Partially correct!</text>",
    "\n</partiallycorrectfeedback>",
    "\n<incorrectfeedback format=\"html\">",
    "\n  <text>Incorrect!</text>",
    "\n</incorrectfeedback>",
    "\n<shownumcorrect/>",
    "\n<subquestion format=\"html\">",
    "\n  <text><![CDATA[<p>Question 1<br></p>]]></text>",
    "\n  <answer>","\n    <text>Answer 1</text>",
    "\n  </answer>","\n</subquestion>",
    "\n<subquestion format=\"html\">",
    "\n  <text><![CDATA[<p>Question 2|Answer 2<br></p>]]></text>",
    "\n  <answer>",
    "\n    <text>NA</text>",
    "\n  </answer>",
    "\n</subquestion>",
    "\n<subquestion format=\"html\">",
    "\n  <text><![CDATA[<p>Question 3|Answer 3<br></p>]]></text>",
    "\n  <answer>",
    "\n    <text>NA</text>",
    "\n  </answer>",
    "\n</subquestion>"
  )

  # Run the function
  result <- generate_matching(
    answer = answer,
    a_values = a_values,
    correct_feedback = correct_feedback,
    incorrect_feedback = incorrect_feedback,
    partially_correct_feedback = partially_correct_feedback
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

