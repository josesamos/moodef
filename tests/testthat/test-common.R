# Tests for string_to_vector -----------------------------------------------
test_that("string_to_vector works correctly", {
  expect_null(string_to_vector(""))
  expect_equal(string_to_vector("a<|>b<|>c"), c("a", "b", "c"))
  expect_equal(string_to_vector("single_value"), "single_value")
  expect_equal(string_to_vector("<|>a<|>"), c("", "a"))
})

# Tests for string_to_string_vector ----------------------------------------
test_that("string_to_string_vector works correctly", {
  expect_equal(string_to_string_vector(""), '""')
  expect_equal(string_to_string_vector("a<|>b<|>c"), 'c("a", "b", "c")')
  expect_equal(string_to_string_vector("single_value"), '"single_value"')
  expect_equal(string_to_string_vector("<|>a<|>"), 'c("", "a")')
  expect_equal(string_to_string_vector(NULL), '""')
})

# Tests for adapt_image ----------------------------------------------------
test_that("adapt_image works correctly", {
  # Mock image file path
  image_file <- system.file("extdata", "divide.png", package = "moodef")

  # Test resizing
  resized_image <- adapt_image(image_file, width = 800, height = 600)
  expect_true(file.exists(resized_image))

  resized_info <- magick::image_info(magick::image_read(resized_image))
  expect_equal(resized_info$width, 800)
  expect_equal(resized_info$height, 600)
})

# Tests for is_numeric -----------------------------------------------------
test_that("is_numeric works correctly", {
  expect_true(is_numeric("123"))
  expect_true(is_numeric("123.45"))
  expect_false(is_numeric("abc"))
  expect_false(is_numeric("123a"))
  expect_true(is_numeric("123e4"))
  expect_false(is_numeric(NA))
})

# Tests for has_gaps -------------------------------------------------------
test_that("has_gaps works correctly", {
  expect_true(has_gaps("This string has [[1]] and [[2]]"))
  expect_false(has_gaps("This string has [[1]] but no second gap"))
  expect_false(has_gaps("No gaps here"))
  expect_false(has_gaps(""))
})
