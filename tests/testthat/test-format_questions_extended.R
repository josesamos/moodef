test_that("validate_and_adjust_dataframe handles correct structure", {
  column_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "fb_a_3", "fb_a_4", "tag_1",
    "tag_2", "tag_3"
  )
  df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(df) <- column_names

  result <- validate_and_adjust_dataframe(df)

  # Check that the dataframe is unchanged (valid structure)
  expect_equal(names(result), column_names)
})

test_that("validate_and_adjust_dataframe warns on missing columns", {
  column_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "tag_1", "tag_2", "tag_3"
  ) # Missing some "fb_a_*" fields
  df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(df) <- column_names

  # Expect a warning about the mismatch in structure
  expect_warning(result <- validate_and_adjust_dataframe(df),
                 "Mismatch: 4 columns after 'answer' but 2 columns after 'fb_answer'.")

  # Original dataframe should remain unchanged
  expect_equal(names(result), column_names)
})

test_that("validate_and_adjust_dataframe renames columns correctly", {
  column_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "col1", "col2", "col3",
    "col4", "fb_answer", "fcol1", "fcol2", "fcol3", "fcol4", "tag_1",
    "tag_2", "tag_3"
  ) # Unconventional names for dynamic sections
  df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(df) <- column_names

  result <- validate_and_adjust_dataframe(df)

  # Expected names after renaming
  expected_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "fb_a_3", "fb_a_4", "tag_1",
    "tag_2", "tag_3"
  )
  expect_equal(names(result), expected_names)
})

test_that("validate_and_adjust_dataframe detects inconsistent sections", {
  column_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "tag_1", "tag_2", "tag_3"
  ) # Different number of columns between answer and fb_answer
  df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(df) <- column_names

  # Expect a warning about mismatch in the number of columns
  expect_warning(result <- validate_and_adjust_dataframe(df),
                 "Mismatch: 4 columns after 'answer' but 2 columns after 'fb_answer'.")
})

test_that("validate_and_adjust_dataframe works with extra columns", {
  column_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "fb_a_3", "fb_a_4", "tag_1",
    "tag_2", "tag_3", "extra_1", "extra_2"
  ) # Extra columns at the end
  df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(df) <- column_names

  result <- validate_and_adjust_dataframe(df)

  # Expected names: extra columns are changed
  expected_names <- c(
    "category", "type", "id", "name", "author", "fb_general", "fb_correct", "fb_partially", "fb_incorrect",
    "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3",
    "a_4", "fb_answer", "fb_a_1", "fb_a_2", "fb_a_3", "fb_a_4", "tag_1",
    "tag_2", "tag_3", "tag_4", "tag_5"
  )
  expect_equal(names(result), expected_names)
})

test_that("validate_and_adjust_dataframe handles invalid 'type' values", {
  # Define mock allowed types
  allowed_types <- c("multiple_choice", "short_answer", "essay")
  simplified_types <- c("mc", "sa", "e")

  # Create a data frame with invalid 'type' values
  df <- data.frame(
    category = c("Math", "Science"),
    type = c("invalid_type_1", "invalid_type_2"),
    id = c("q1", "q2"),
    name = c("Addition", "Physics"),
    author = c("John Doe", "Jane Smith"),
    fb_general = c("", ""),
    fb_correct = c("", ""),
    fb_partially = c("", ""),
    fb_incorrect = c("", ""),
    question = c("What is 2 + 2?", "What is the speed of light?"),
    image = c("", ""),
    image_alt = c("", ""),
    answer = c("4", "299,792,458 m/s"),
    fb_answer = c("", ""),
    tag_1 = c("math", "science"),
    stringsAsFactors = FALSE
  )

  # Capture warnings
  warnings <- capture_warnings({
    adjusted_df <- validate_and_adjust_dataframe(df)
  })

  # Check if the warning for invalid 'type' values is triggered
  expect_true(any(grepl("The 'type' column contains invalid values", warnings)))

  # Verify that invalid 'type' values are correctly identified in the warning
  expect_true(any(grepl("invalid_type_1, invalid_type_2", warnings)))

  # Ensure the data frame is returned unchanged since errors exist
  expect_equal(adjusted_df, df)
})

test_that("get_non_empty_fields_by_prefix works correctly with valid input", {
  df <- data.frame(
    category = c("cat1", "cat2"),
    type = c("type1", "type2"),
    id = c(1, 2),
    a_1 = c("val1", ""),
    a_2 = c("", "val2"),
    fb_a_1 = c("fb_val1", ""),
    fb_a_2 = c("", "fb_val2"),
    tag_1 = c("tag1", "tag2"),
    tag_2 = c("", ""),
    stringsAsFactors = FALSE
  )

  # Test prefix "a_"
  expect_equal(get_non_empty_fields_by_prefix(df, 1, "a_"), c("val1"))
  expect_equal(get_non_empty_fields_by_prefix(df, 2, "a_"), c("val2"))

  # Test prefix "fb_a_"
  expect_equal(get_non_empty_fields_by_prefix(df, 1, "fb_a_"), c("fb_val1"))
  expect_equal(get_non_empty_fields_by_prefix(df, 2, "fb_a_"), c("fb_val2"))

  # Test prefix "tag_"
  expect_equal(get_non_empty_fields_by_prefix(df, 1, "tag_"), c("tag1"))
  expect_equal(get_non_empty_fields_by_prefix(df, 2, "tag_"), c("tag2"))
})

test_that("get_non_empty_fields_by_prefix returns empty vector for no matches", {
  df <- data.frame(
    category = c("cat1", "cat2"),
    type = c("type1", "type2"),
    id = c(1, 2),
    a_1 = c("val1", ""),
    stringsAsFactors = FALSE
  )

  expect_equal(get_non_empty_fields_by_prefix(df, 1, "b_"), NULL)
  expect_equal(get_non_empty_fields_by_prefix(df, 2, "tag_"), NULL)
})

test_that("get_non_empty_fields_by_prefix handles empty rows gracefully", {
  df <- data.frame(
    a_1 = c("", ""),
    a_2 = c("", ""),
    stringsAsFactors = FALSE
  )

  expect_equal(get_non_empty_fields_by_prefix(df, 1, "a_"), NULL)
  expect_equal(get_non_empty_fields_by_prefix(df, 2, "a_"), NULL)
})

test_that("get_non_empty_fields_by_prefix checks row index validity", {
  df <- data.frame(
    a_1 = c("val1", "val2"),
    stringsAsFactors = FALSE
  )

  expect_error(get_non_empty_fields_by_prefix(df, 0, "a_"), "Row index is out of bounds.")
  expect_error(get_non_empty_fields_by_prefix(df, 3, "a_"), "Row index is out of bounds.")
})

test_that("get_non_empty_fields_by_prefix works with no matching columns", {
  df <- data.frame(
    category = c("cat1", "cat2"),
    type = c("type1", "type2"),
    stringsAsFactors = FALSE
  )

  expect_equal(get_non_empty_fields_by_prefix(df, 1, "a_"), NULL)
})


test_that("extended_format_questions", {
  qc <- question_category(category = 'Initial test') |>
    define_extended_question(
      question = 'What are the basic arithmetic operations?',
      answer = 'Addition, subtraction, multiplication and division.',
      a_1 = 'Addition and subtraction.',
      a_2 = 'Addition, subtraction, multiplication, division and square root.'
    )

  r <- extended_format_questions(qc)

  expect_equal(
    r,
    structure(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n<question type=\"category\">\n  <category> <text>$course$/top/Initial test</text> </category>\n  <info format=\"html\"> <text></text> </info>\n  <idnumber></idnumber>\n</question>\n<question type=\"multichoice\">\n  <name> <text>q_001_multichoice_what_are_the_basic_arithmetic_operations</text> </name>\n  \n<questiontext format=\"html\">\n  <text><![CDATA[\n     \n     \n     \n     <p>What are the basic arithmetic operations?</p>]]></text>\n     \n</questiontext>\n<generalfeedback format=\"html\">\n  <text></text>\n</generalfeedback>\n<defaultgrade>1.0</defaultgrade>\n<penalty>0.5</penalty>\n<hidden>0</hidden>\n<idnumber></idnumber>\n  \n<single>true</single>\n<shuffleanswers>true</shuffleanswers>\n<answernumbering>abc</answernumbering>\n<showstandardinstruction>0</showstandardinstruction>\n<correctfeedback format=\"moodle_auto_format\"> <text>Correct.</text> </correctfeedback>\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text>Partially correct.</text> </partiallycorrectfeedback>\n<incorrectfeedback format=\"moodle_auto_format\"> <text>Incorrect.</text> </incorrectfeedback>\n<answer fraction=\"100\" format=\"html\">\n   <text>Addition, subtraction, multiplication and division.</text>\n   <feedback format=\"html\"> <text>Correct.</text> </feedback>\n</answer>\n<answer fraction=\"-50.000000000000000\" format=\"html\">\n   <text>Addition and subtraction.</text>\n   <feedback format=\"html\"> <text>Incorrect.</text> </feedback>\n</answer>\n<answer fraction=\"-50.000000000000000\" format=\"html\">\n   <text>Addition, subtraction, multiplication, division and square root.</text>\n   <feedback format=\"html\"> <text>Incorrect.</text> </feedback>\n</answer>\n  \n</question>\n</quiz>",
      class = c("glue", "character")
    )
  )
})
