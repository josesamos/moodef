test_that("xml_question generates correct XML structure with all inputs", {
  # Inputs
  type <- "multichoice"
  name <- "<name><text>Sample Question</text></name>"
  questiontext <- '<questiontext format="html"><text><![CDATA[What is 2 + 2?]]></text></questiontext>'
  question_body <- '<answer fraction="100"><text>4</text></answer>'
  question_tags <- '<tags><tag><text>math</text></tag></tags>'

  # Expected output
  expected_output <- structure(
    "\n<question type=\"multichoice\">\n  <name><text>Sample Question</text></name>\n  <questiontext format=\"html\"><text><![CDATA[What is 2 + 2?]]></text></questiontext>\n  <answer fraction=\"100\"><text>4</text></answer>\n  <tags><tag><text>math</text></tag></tags>\n</question>",
    class = c("glue", "character")
  )

  # Run the function
  result <- xml_question(type, name, questiontext, question_body, question_tags)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})


test_that("xml_question_category generates correct XML for a valid category", {
  # Input
  category <- "Mathematics"

  # Expected output
  expected_output <- glue::glue(
    '

  <question type="category">
    <category> <text>$course$/top/{category}</text> </category>
    <info format="html"> <text></text> </info>
    <idnumber></idnumber>
  </question>
'
  )

  # Run the function
  result <- xml_question_category(category)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})
