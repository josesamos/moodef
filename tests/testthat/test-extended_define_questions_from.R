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
