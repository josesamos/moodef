test_that("generate_numerical generates correct XML for a single answer", {
  # Inputs
  answer <- c("42", "0.01")
  a_values <- list()
  fb_answer <- "Correct!"
  fb_a_values <- NULL

  # Expected output
  expected_structure <- paste0(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>42</text>",
    "\n  <feedback format=\"html\">",
    "\n    <text>Correct!</text>",
    "\n  </feedback>",
    "\n  <tolerance>0.01</tolerance>",
    "\n</answer>",
    "\n    <unitgradingtype>0</unitgradingtype>",
    "\n    <unitpenalty>0.1000000</unitpenalty>",
    "\n    <showunits>3</showunits>",
    "\n    <unitsleft>0</unitsleft>"
  )

  # Run the function
  result <- generate_numerical(
    answer = answer,
    a_values = a_values,
    fb_answer = fb_answer,
    fb_a_values = fb_a_values
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_numerical handles multiple answers with feedback", {
  # Inputs
  answer <- c("100", "0.5")
  a_values <- c("90<|>0.1", "110<|>0.1")
  fb_answer <- "Correct answer!"
  fb_a_values <- c("Close enough!", "Too high!")

  # Expected output
  expected_structure <- paste0(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>100</text>",
    "\n  <feedback format=\"html\">",
    "\n    <text>Correct answer!</text>",
    "\n  </feedback>","\n  <tolerance>0.5</tolerance>",
    "\n</answer><answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>90</text>","\n  <feedback format=\"html\">",
    "\n    <text>Close enough!</text>",
    "\n  </feedback>",
    "\n  <tolerance>0.1</tolerance>",
    "\n</answer><answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>110</text>","\n  <feedback format=\"html\">",
    "\n    <text>Too high!</text>",
    "\n  </feedback>",
    "\n  <tolerance>0.1</tolerance>",
    "\n</answer>",
    "\n    <unitgradingtype>0</unitgradingtype>",
    "\n    <unitpenalty>0.1000000</unitpenalty>",
    "\n    <showunits>3</showunits>",
    "\n    <unitsleft>0</unitsleft>"
  )

  # Run the function
  result <- generate_numerical(
    answer = answer,
    a_values = a_values,
    fb_answer = fb_answer,
    fb_a_values = fb_a_values
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_numerical handles empty feedback and tolerance defaults to 0", {
  # Inputs
  answer <- c("50")
  a_values <- c("60")
  fb_answer <- ""
  fb_a_values <- NULL

  # Expected output
  expected_structure <- paste0(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>50</text>",
    "\n  <feedback format=\"html\">",
    "\n    <text></text>",
    "\n  </feedback>",
    "\n  <tolerance>0</tolerance>",
    "\n</answer><answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text>60</text>",
    "\n  <feedback format=\"html\">",
    "\n    <text></text>",
    "\n  </feedback>",
    "\n  <tolerance>0</tolerance>",
    "\n</answer>",
    "\n    <unitgradingtype>0</unitgradingtype>",
    "\n    <unitpenalty>0.1000000</unitpenalty>",
    "\n    <showunits>3</showunits>",
    "\n    <unitsleft>0</unitsleft>"
  )

  # Run the function
  result <- generate_numerical(
    answer = answer,
    a_values = a_values,
    fb_answer = fb_answer,
    fb_a_values = fb_a_values
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_numerical handles empty inputs gracefully", {
  # Inputs
  answer <- ""
  a_values <- ""
  fb_answer <- ""
  fb_a_values <- NULL

  # Expected output
  expected_structure <- paste0(
    "\n<answer fraction=\"100\" format=\"moodle_auto_format\">",
    "\n  <text></text>",
    "\n  <feedback format=\"html\">",
    "\n    <text></text>",
    "\n  </feedback>",
    "\n  <tolerance>0</tolerance>",
    "\n</answer>",
    "\n    <unitgradingtype>0</unitgradingtype>",
    "\n    <unitpenalty>0.1000000</unitpenalty>",
    "\n    <showunits>3</showunits>",
    "\n    <unitsleft>0</unitsleft>"
  )

  # Run the function
  result <- generate_numerical(
    answer = answer,
    a_values = a_values,
    fb_answer = fb_answer,
    fb_a_values = fb_a_values
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
