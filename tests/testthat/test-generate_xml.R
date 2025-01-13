test_that("generate_xml", {
  file <- system.file("extdata", "questions.csv", package = "moodef")
  qc <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_csv(file = file)

  xml <- qc |>
    generate_xml()

  qc2 <- qc |>
    generate_xml_file(file = tempfile(fileext = '.xml'))

  file <-
    system.file("extdata", "questions_image.csv", package = "moodef")
  df <- read_question_csv(file = file)
  df[1, 'image'] <-
    system.file("extdata", "divide.png", package = "moodef")
  df[2, 'image'] <-
    system.file("extdata", "ops.png", package = "moodef")
  qc3 <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_data_frame(df)
  xml3 <- qc3 |>
    generate_xml()

  expect_equal(qc, qc2)


})
