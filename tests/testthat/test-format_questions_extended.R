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
})
