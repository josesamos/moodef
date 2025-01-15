test_that("filter_non_empty_answers removes empty answers and trims whitespace", {
  # Test with mixed empty and non-empty strings
  result <- filter_non_empty_answers(" answer1 ", "", "   ", "answer2")
  expect_equal(result, c("answer1", "answer2"))

  # Test with only empty strings
  result <- filter_non_empty_answers("", "   ", "")
  expect_equal(result, NULL)

  # Test with only non-empty strings
  result <- filter_non_empty_answers("answer1", "answer2", " answer3 ")
  expect_equal(result, c("answer1", "answer2", "answer3"))

  # Test with no inputs
  result <- filter_non_empty_answers()
  expect_equal(result, NULL)

  # Test with numeric inputs (converted to character implicitly)
  result <- filter_non_empty_answers(1, 0, 3.14, "")
  expect_equal(result, c("1", "0", "3.14"))
})

test_that("determine_question_type correctly identifies question types", {
  # Numerical questions
  result <- determine_question_type("", "What is 5+3?", "8", list())
  expect_equal(result, "numerical")

  # Single answer, no gaps, type unspecified -> multichoice
  result <- determine_question_type("", "Choose the correct answer", "A", "B")
  expect_equal(result, "multichoice")

  # Single answer, no gaps, type specified -> ordering
  result <- determine_question_type("x", "Arrange these in order", "A", "B")
  expect_equal(result, "ordering")

  # Single answer, with gaps, type unspecified -> ddwtos
  result <- determine_question_type("", "Fill [[1]] in the [[2]]", "answer", "B")
  expect_equal(result, "ddwtos")

  # Single answer, with gaps, type specified -> gapselect
  result <- determine_question_type("x", "Fill [[1]] in the [[2]]", "answer", "B")
  expect_equal(result, "gapselect")

  # Multiple answers -> matching
  result <- determine_question_type("", "Match the following", c("A", "B"), "B")
  expect_equal(result, "matching")

  # Empty answer -> essay
  result <- determine_question_type("", "Write an essay on...", "", NULL)
  expect_equal(result, "essay")

  # Boolean answers -> truefalse
  result <- determine_question_type("", "Is the earth round?", "true", NULL)
  expect_equal(result, "truefalse")

  result <- determine_question_type("", "Is the earth flat?", "FALSE", NULL)
  expect_equal(result, "truefalse")

  # Short answer
  result <- determine_question_type("", "What is the capital of France?", "Paris", NULL)
  expect_equal(result, "shortanswer")
})


test_that("generate_question_body works correctly for each question type", {
  # Numerical
  mock_answer <- "42"
  mock_rest <- list()
  result <- generate_question_body("numerical", mock_answer, mock_rest, NULL, NULL, NULL, NULL)
  expect_equal(result, generate_numerical(mock_answer, mock_rest))

  # Multichoice
  mock_answer <- "A"
  mock_rest <- list("Option B", "Option C")
  result <- generate_question_body("multichoice", mock_answer, mock_rest, "Correct!", "Incorrect!", NULL, NULL)
  expect_equal(result, generate_multichoice(mock_answer, mock_rest, "Correct!", "Incorrect!"))

  # Ordering
  mock_answer <- c("Step 1", "Step 2", "Step 3")
  result <- generate_question_body("ordering", mock_answer, mock_rest, "Good job!", "Try again!", "Almost!", "vertical")
  expect_equal(result, generate_ordering(mock_answer, mock_rest, "Good job!", "Try again!", "Almost!", "vertical"))

  # DDWTOS
  mock_answer <- "Drag this <gap> to here."
  result <- generate_question_body("ddwtos", mock_answer, mock_rest, "Perfect!", "Not quite.", "Close!", NULL)
  expect_equal(result, generate_ddwtos(mock_answer, mock_rest, "Perfect!", "Not quite.", "Close!"))

  # Gapselect
  result <- generate_question_body("gapselect", mock_answer, mock_rest, "Correct!", "Wrong.", "Nearly!", NULL)
  expect_equal(result, generate_gapselect(mock_answer, mock_rest, "Correct!", "Wrong.", "Nearly!"))

  # Matching
  mock_answer <- c("A" = "1", "B" = "2")
  result <- generate_question_body("matching", mock_answer, mock_rest, "Matched!", "Mismatch!", "Partial match!", NULL)
  expect_equal(result, generate_matching(mock_answer, mock_rest, "Matched!", "Mismatch!", "Partial match!"))

  # Essay
  result <- generate_question_body("essay", NULL, NULL, NULL, NULL, NULL, NULL)
  expect_equal(result, generate_essay())

  # True/False
  mock_answer <- "true"
  result <- generate_question_body("truefalse", mock_answer, NULL, NULL, NULL, NULL, NULL)
  expect_equal(result, generate_truefalse(mock_answer))

  # Shortanswer
  mock_answer <- "Answer"
  result <- generate_question_body("shortanswer", mock_answer, NULL, NULL, NULL, NULL, NULL)
  expect_equal(result, generate_shortanswer(mock_answer))
})

test_that("determine_orientation works correctly", {

  # Test when type is 'H'
  expect_equal(determine_orientation("H"), "h", info = "Should return 'h' for type 'H'")

  # Test when type is 'h'
  expect_equal(determine_orientation("h"), "h", info = "Should return 'h' for type 'h'")

  # Test when type is a different character (e.g., 'V')
  expect_equal(determine_orientation("V"), "v", info = "Should return 'v' for any non-'H' or non-'h' type")

  # Test when type is lowercase 'v'
  expect_equal(determine_orientation("v"), "v", info = "Should return 'v' for type 'v'")

  # Test when type is empty
  expect_equal(determine_orientation(""), "v", info = "Should return 'v' for an empty type")

  # Test when type is numeric (as string)
  expect_equal(determine_orientation("1"), "v", info = "Should return 'v' for numeric values passed as strings")

  # Test when type is NULL (should handle it gracefully if wrapped in a higher function)
  expect_error(determine_orientation(NULL), regexp = "argument is of length zero", info = "Should return an error for NULL input")

  # Test for case sensitivity
  expect_equal(determine_orientation("h"), "h", info = "Should treat 'h' as lowercase horizontal")
  expect_equal(determine_orientation("H"), "h", info = "Should treat 'H' as uppercase horizontal")
  expect_equal(determine_orientation("random"), "v", info = "Should return 'v' for unexpected inputs")
})


test_that("get_vector_answer works correctly", {

  # Test when answer is a single value
  expect_equal(get_vector_answer("A"), c("A"),
               info = "Should return a vector with one element for a single value")

  # Test when answer is an empty string
  expect_equal(get_vector_answer(""), c(""),
               info = "Should return a vector with an empty string for an empty input string")
})

test_that("generate_question works correctly for different configurations", {
  file <- system.file("extdata", "questions.csv", package = "moodef")
  qc <-
    question_category(category = 'Initial test', adapt_images = TRUE) |>
    define_questions_from_csv(file = file)

  questions <- format_questions(qc$questions)

  q <- qc$questions[1, ]

  # Asignar cada componente de q a la variable correspondiente
  first_question_number <- q$first_question_number
  copyright <- q$copyright
  license <- q$license
  correct_feedback <- q$correct_feedback
  partially_correct_feedback <- q$partially_correct_feedback
  incorrect_feedback <- q$incorrect_feedback
  adapt_images <- q$adapt_images
  width <- q$width
  height <- q$height
  type <- q$type
  question <- q$question
  image <- q$image
  image_alt <- q$image_alt
  answer <- q$answer

  expected_result <- structure(
    "\n<question type=\"multichoice\">\n  <name> <text>q_001_multichoice_what_are_the_basic_arithmetic_operations</text> </name>\n  \n<questiontext format=\"html\">\n  <text><![CDATA[\n     \n     \n     \n     <p>What are the basic arithmetic operations?</p>]]></text>\n     \n</questiontext>\n<generalfeedback format=\"html\">\n  <text></text>\n</generalfeedback>\n<defaultgrade>1.0</defaultgrade>\n<penalty>0.5</penalty>\n<hidden>0</hidden>\n<idnumber></idnumber>\n  \n<single>true</single>\n<shuffleanswers>true</shuffleanswers>\n<answernumbering>abc</answernumbering>\n<showstandardinstruction>0</showstandardinstruction>\n<correctfeedback format=\"moodle_auto_format\"> <text>Correct.</text> </correctfeedback>\n<partiallycorrectfeedback format=\"moodle_auto_format\"> <text></text> </partiallycorrectfeedback>\n<incorrectfeedback format=\"moodle_auto_format\"> <text>Incorrect.</text> </incorrectfeedback>\n<answer fraction=\"100\" format=\"html\">\n   <text>Addition, subtraction, multiplication and division.</text>\n   <feedback format=\"html\"> <text>Correct.</text> </feedback>\n</answer>\n<answer fraction=\"-50.000000000000000\" format=\"html\">\n   <text>Addition and subtraction.</text>\n   <feedback format=\"html\"> <text>Incorrect.</text> </feedback>\n</answer>\n<answer fraction=\"-50.000000000000000\" format=\"html\">\n   <text>Addition, subtraction, multiplication, division and square root.</text>\n   <feedback format=\"html\"> <text>Incorrect.</text> </feedback>\n</answer>\n  \n</question>",
    class = c("glue", "character")
  )

  result <- generate_question(first_question_number, copyright, license, correct_feedback,
                              partially_correct_feedback, incorrect_feedback, adapt_images,
                              width, height, type, question, image, image_alt, answer,
                              q$a_1,
                              q$a_2,
                              q$a_3)

  expect_equal(result, expected_result)
})
