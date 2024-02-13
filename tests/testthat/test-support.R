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
  f3 <- create_question_csv(file = file3)

  file4 <-
    system.file("extdata", "questions.xlsx", package = "moodef")
  df3 <- read_question_excel(file = file4)
  names(df3) <- names(df2)

  df4 <- read_question_excel(file = file4, sheet_name = 'Hoja1')
  names(df4) <- names(df2)

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
                 "6",
                 "7",
                 "8"
               ))

  expect_equal(df2, df3)

  expect_equal(df2, df4)

})
