
test_that("Vignette works correctly", {

qc <- question_category(category = 'Initial test',
                        copyright = 'Copyright © 2025 Universidad de Granada',
                        license = 'License Creative Commons Attribution-ShareAlike 4.0',
                        author = 'Jose Samos')


## -----------------------------------------------------------------------------------------------------------------
qc <- question_category(category = 'Initial test',
                        first_question_number = 1,
                        copyright = 'Copyright © 2025 Universidad de Granada',
                        license = 'License Creative Commons Attribution-ShareAlike 4.0',
                        author = 'Jose Samos',
                        penalty = 0,
                        correct_feedback = 'Correct.',
                        partially_correct_feedback = 'Partially correct.',
                        incorrect_feedback = 'Incorrect.',
                        adapt_images = TRUE,
                        width = 800,
                        height = 600)


## ----example1-----------------------------------------------------------------------------------------------------
qc <- qc |>
  define_question(
    question = 'Describe the addition operation.'
  )


## ----example2-----------------------------------------------------------------------------------------------------
qc <- qc |>
  define_question(
    type = 'v',
    question = 'Place the name of the operations as they appear in the figure.',
    image = system.file("extdata", "ops.png", package = "moodef"),
    image_alt = 'Operations',
    answer = 'Addition',
    a_1 = 'Multiplication',
    a_2 = 'Division',
    a_3 = 'Subtraction'
  )


## ----example2-2---------------------------------------------------------------------------------------------------
qc <- qc |>
  define_extended_question(
    type = 'v',
    question = 'Place the name of the operations as they appear in the figure.',
    image = system.file("extdata", "ops.png", package = "moodef"),
    image_alt = 'Operations',
    answer = 'Addition',
    a_1 = 'Multiplication',
    a_2 = 'Division',
    a_3 = 'Subtraction'
  )


## ----example2-3---------------------------------------------------------------------------------------------------
qc <- qc |>
  define_extended_question(
    category = 'Initial test',
    type = 'ordering<|>v',
    penalty = 0,
    id = '',
    name = 'q_001_ordering_v_place_the_name_of_the_operations_as_they',
    author = 'Jose Samos',
    fb_general = '',
    fb_correct = 'Correct.',
    fb_partially = 'Partially correct.',
    fb_incorrect = 'Incorrect.',
    question = 'Place the name of the operations as they appear in the figure.',
    image = system.file("extdata", "ops.png", package = "moodef"),
    image_alt = 'Operations',
    answer = 'Addition',
    a_1 = 'Multiplication',
    a_2 = 'Division',
    a_3 = 'Subtraction',
    fb_a_1 = '',
    fb_a_2 = '',
    fb_a_3 = '',
    tag_1 = ''
  )


## -----------------------------------------------------------------------------------------------------------------
file <- create_question_csv(file = tempfile(fileext = '.csv'))


## ----echo=FALSE, results = "asis"---------------------------------------------------------------------------------
file <- system.file("extdata", "questions.csv", package = "moodef")
df <- read_question_csv(file = file)
df$a_3 <- NULL


## -----------------------------------------------------------------------------------------------------------------
file <- system.file("extdata", "questions.csv", package = "moodef")
qc <- qc |>
  define_questions_from_csv(file)


## -----------------------------------------------------------------------------------------------------------------
file <- system.file("extdata", "questions_image.csv", package = "moodef")
df <- read_question_csv(file = file)



## -----------------------------------------------------------------------------------------------------------------
df[1, 'image'] <- system.file("extdata", "divide.png", package = "moodef")
df[2, 'image'] <- system.file("extdata", "ops.png", package = "moodef")


## -----------------------------------------------------------------------------------------------------------------
s <- vector_to_string(c('Addition', '+'))
s


## -----------------------------------------------------------------------------------------------------------------
qc <- qc |>
  define_questions_from_data_frame(df)


## -----------------------------------------------------------------------------------------------------------------
file <- tempfile(fileext = '.xml')
qc <- qc |>
  generate_xml_file(file)


## -----------------------------------------------------------------------------------------------------------------
file.size(file)

})
