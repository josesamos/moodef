test_that("support", {
  s <- vector_to_string(c('Addition', '+'))
  s2 <- vector_to_string(NULL)
  s3 <- vector_to_string("")

  df <- create_question_data_frame()

  file <- tempfile(fileext = '.csv')
  f <- create_question_csv(file = file)

  file2 <-
    system.file("extdata", "questions.csv", package = "moodef")
  df2 <- read_question_csv(file = file2)

  file3 <- tempfile(fileext = '.xlsx')
  f3 <- create_question_excel(file = file3)

  file4 <-
    system.file("extdata", "questions.xlsx", package = "moodef")
  df3 <- read_question_excel(file = file4)
  names(df3) <- names(df2)

  df4 <- read_question_excel(file = file4, sheet_name = 'Hoja1')
  names(df4) <- names(df2)

  df5 <- read_question_excel(file = file4, sheet_index = 1)
  names(df5) <- names(df2)

  expect_equal(s, "Addition<|>+")
  expect_equal(s2, "")
  expect_equal(s3, "")

  expect_equal(df,
               structure(
                 list(
                   type = character(0),
                   question = character(0),
                   image = character(0),
                   image_alt = character(0),
                   answer = character(0),
                   a_1 = character(0),
                   a_2 = character(0),
                   a_3 = character(0)
                 ),
                 class = "data.frame",
                 row.names = integer(0)
               ))

  expect_equal(file, f)

  expect_equal(file3, f3)

  expect_equal(nrow(df2), 11)

  expect_equal(names(df2),
               c(
                 "type",
                 "question",
                 "image",
                 "image_alt",
                 "answer",
                 "a_1",
                 "a_2",
                 "a_3"
               ))

  expect_equal(df2, df3)

  expect_equal(df2, df4)

  expect_equal(df2, df5)

})


test_that("create_question_data_frame creates correct data frame", {
  df <- create_question_data_frame()
  expect_equal(names(df), c("type", "question", "image", "image_alt", "answer", "a_1", "a_2", "a_3"))
  expect_equal(nrow(df), 0)

  df_extended <- create_question_data_frame(extended = TRUE)
  expect_true("category" %in% names(df_extended))
  expect_equal(nrow(df_extended), 0)
})

test_that("create_question_csv creates a valid CSV file", {
  file <- tempfile(fileext = ".csv")
  result <- create_question_csv(file)
  expect_true(file.exists(result))

  df <- read.csv(file, stringsAsFactors = FALSE)
  expect_equal(ncol(df), 8) # standard number of columns
})

test_that("read_question_csv reads valid CSV files", {
  file <- tempfile(fileext = ".csv")
  create_question_csv(file)

  df <- read_question_csv(file)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 8) # spected number of columns
})


test_that("process_question_dataframe processes data frame correctly", {
  df <- data.frame(a = c(NA, "value"), b = c("text", NA), stringsAsFactors = FALSE)
  result <- process_question_dataframe(df)

  expect_true(all(!is.na(result)))
  expect_equal(names(result), c("a", "b")) # Snake_case ya coincide en este caso
})

test_that("process_question_dataframe handles single-row dataframes correctly", {
  # Input dataframe with a single row
  df <- data.frame(
    ColumnA = c("Value1"),
    ColumnB = c(NA),
    ColumnC = c("Value3"),
    stringsAsFactors = FALSE
  )

  # Expected output dataframe
  expected_df <- as.data.frame(tibble::tibble(
    column_a = "Value1",
    column_b = "",
    column_c = "Value3"
  ))

  # Call the function
  result <- process_question_dataframe(df)

  # Check that the result matches the expected dataframe
  expect_equal(result, expected_df)
})

test_that("process_question_dataframe converts all columns to character type for a single row", {
  # Input dataframe with mixed types
  df <- data.frame(
    NumericColumn = c(123),
    CharacterColumn = c("Some text"),
    LogicalColumn = c(TRUE),
    stringsAsFactors = FALSE
  )

  # Expected output dataframe
  expected_df <- as.data.frame(tibble::tibble(
    numeric_column = "123",
    character_column = "Some text",
    logical_column = "TRUE"
  ))

  # Call the function
  result <- process_question_dataframe(df)

  # Check column types
  expect_true(all(sapply(result, is.character)))
  expect_equal(result, expected_df)
})

