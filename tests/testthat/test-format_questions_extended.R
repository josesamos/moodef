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


