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
