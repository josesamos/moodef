test_that("xml_question generates correct XML structure with all inputs", {
  # Inputs
  type <- "multichoice"
  name <- "<name><text>Sample Question</text></name>"
  questiontext <- '<questiontext format="html"><text><![CDATA[What is 2 + 2?]]></text></questiontext>'
  question_body <- '<answer fraction="100"><text>4</text></answer>'
  question_tags <- '<tags><tag><text>math</text></tag></tags>'

  # Expected output
  expected_output <- structure(
    "\n<question type=\"multichoice\">\n  <name><text>Sample Question</text></name>\n  <questiontext format=\"html\"><text><![CDATA[What is 2 + 2?]]></text></questiontext>\n  <answer fraction=\"100\"><text>4</text></answer>\n  <tags><tag><text>math</text></tag></tags>\n</question>",
    class = c("glue", "character")
  )

  # Run the function
  result <- xml_question(type, name, questiontext, question_body, question_tags)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})


test_that("xml_question_tags returns empty string for NULL input", {
  # Input
  tag_values <- NULL

  # Expected output
  expected_output <- ""

  # Run the function
  result <- xml_question_tags(tag_values)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_question_tags returns empty string for empty vector input", {
  # Input
  tag_values <- character(0)

  # Expected output
  expected_output <- ""

  # Run the function
  result <- xml_question_tags(tag_values)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_question_tags generates correct XML for a single tag", {
  # Input
  tag_values <- "math"

  # Expected output
  expected_output <- paste0(
    "<tags>\n",
    "    <tag><text>math</text></tag>\n",
    "  </tags>"
  )

  # Run the function
  result <- xml_question_tags(tag_values)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_question_tags generates correct XML for multiple tags", {
  # Input
  tag_values <- c("math", "science", "history")

  # Expected output
  expected_output <- paste0(
    "<tags>\n",
    "    <tag><text>math</text></tag>\n",
    "    <tag><text>science</text></tag>\n",
    "    <tag><text>history</text></tag>\n",
    "  </tags>"
  )

  # Run the function
  result <- xml_question_tags(tag_values)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_question_category generates correct XML for a valid category", {
  # Input
  category <- "Mathematics"

  # Expected output
  expected_output <- glue::glue(
    '

  <question type="category">
    <category> <text>$course$/top/{category}</text> </category>
    <info format="html"> <text></text> </info>
    <idnumber></idnumber>
  </question>
'
  )

  # Run the function
  result <- xml_question_category(category)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_question_category handles empty category name", {
  # Input
  category <- ""

  # Expected output
  expected_output <- glue::glue(
    '

  <question type="category">
    <category> <text>$course$/top/{category}</text> </category>
    <info format="html"> <text></text> </info>
    <idnumber></idnumber>
  </question>
'
  )

  # Run the function
  result <- xml_question_category(category)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})

test_that("xml_image generates correct output for valid image input", {
  # Load the test image
  image_path <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Test Image"

  # Call the function
  result <- xml_image(image = image_path, image_alt = image_alt, adapt_images = FALSE)

  # Check the generated img and fimg
  expect_match(result$img, "<p><img src=\"@@PLUGINFILE@@/divide.png\" alt=\"Test Image\"", fixed = TRUE)
  expect_match(result$fimg, "<file name=\"divide.png\" path=\"/\" encoding=\"base64\">", fixed = TRUE)
})

test_that("xml_image handles empty image input gracefully", {
  # Call the function with empty image
  result <- xml_image(image = "", image_alt = "No Image", adapt_images = FALSE)

  # Check the output
  expect_equal(result$img, "")
  expect_equal(result$fimg, "")
})

test_that("xml_image correctly adapts image dimensions when adapt_images is TRUE", {
  # Load the test image
  image_path <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Test Image"

  # Mock the adapt_image function
  mock_adapt_image <- function(image_file, width, height) {
    # Return a modified image path for testing
    return(image_file)
  }
  with_mocked_bindings(
    adapt_image = mock_adapt_image,
    {
      # Call the function
      result <- xml_image(image = image_path, image_alt = image_alt, adapt_images = TRUE, width = 100, height = 100)
    }
  )

  # Check the generated img and fimg
  expect_match(result$img, "width=\"100\" height=\"100\"", fixed = TRUE)
})

test_that("xml_image handles images with no alternative text", {
  # Load the test image
  image_path <- system.file("extdata", "divide.png", package = "moodef")

  # Call the function without image_alt
  result <- xml_image(image = image_path, image_alt = "", adapt_images = FALSE)

  # Check the generated img
  expect_match(result$img, "<p><img src=\"@@PLUGINFILE@@/divide.png\" alt=\"\"", fixed = TRUE)
})

test_that("xml_image handles special characters in image_alt", {
  # Load the test image
  image_path <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Special < & > Characters"

  # Call the function
  result <- xml_image(image = image_path, image_alt = image_alt, adapt_images = FALSE)

  # Check that special characters are properly handled
  expect_match(result$img, "alt=\"Special < & > Characters\"", fixed = TRUE)
})

test_that("xml_questiontext generates correct question text with all inputs", {
  # Inputs
  copyright <- "Copyright Info"
  license <- "License Info"
  adapt_images <- FALSE
  width <- 200
  height <- 100
  question <- "What is the capital of France?"
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Sample Image"
  type <- "multichoice"
  author <- "John Doe"
  fb_general <- "Paris is the capital of France."
  idnumber <- "Q001"

  # Call the function
  result <- xml_questiontext(copyright, license, adapt_images, width, height,
                             question, image, image_alt, type, author,
                             fb_general, idnumber)

  # Expectations
  expect_match(result, "<!-- Copyright Info -->", fixed = TRUE)
  expect_match(result, "<!-- License Info -->", fixed = TRUE)
  expect_match(result, "<!-- Author: John Doe -->", fixed = TRUE)
  expect_match(result, "<p>What is the capital of France?</p>", fixed = TRUE)
  expect_match(result, "<![CDATA[<p>Paris is the capital of France.</p>]]>", fixed = TRUE)
  expect_match(result, "<defaultgrade>1.0</defaultgrade>", fixed = TRUE)
  expect_match(result, "<penalty>0.5</penalty>", fixed = TRUE)
  expect_match(result, "<idnumber>Q001</idnumber>", fixed = TRUE)
})

test_that("xml_questiontext handles missing optional inputs gracefully", {
  # Inputs
  copyright <- ""
  license <- ""
  adapt_images <- FALSE
  width <- 200
  height <- 100
  question <- "What is 2 + 2?"
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Math Image"
  type <- "truefalse"
  author <- ""
  fb_general <- ""
  idnumber <- ""

  # Call the function
  result <- xml_questiontext(copyright, license, adapt_images, width, height,
                             question, image, image_alt, type, author,
                             fb_general, idnumber)

  # Expectations
  expect_false(grepl("<!-- Copyright Info -->", result))
  expect_false(grepl("<!-- License Info -->", result))
  expect_false(grepl("<!-- Author:", result))
  expect_match(result, "<p>What is 2 + 2?</p>", fixed = TRUE)
  expect_match(result, "<defaultgrade>1.0</defaultgrade>", fixed = TRUE)
  expect_match(result, "<penalty>1.0</penalty>", fixed = TRUE)
  expect_match(result, "<idnumber></idnumber>", fixed = TRUE)
})

test_that("xml_questiontext handles type 'ddmarker' correctly", {
  # Inputs
  copyright <- "Copyright Info"
  license <- "License Info"
  adapt_images <- FALSE
  width <- 200
  height <- 100
  question <- "Place the marker at the correct location."
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Marker Image"
  type <- "ddmarker"
  author <- "Jane Doe"
  fb_general <- "Drag and drop question."
  idnumber <- "Q002"

  # Call the function
  result <- xml_questiontext(copyright, license, adapt_images, width, height,
                             question, image, image_alt, type, author,
                             fb_general, idnumber)

  # Expectations
  expect_false(grepl("<p><img", result)) # Image should not be included for 'ddmarker'
  expect_match(result, "<penalty>0.3333333</penalty>", fixed = TRUE)
})

test_that("xml_questiontext handles empty feedback correctly", {
  # Inputs
  copyright <- "Copyright Info"
  license <- "License Info"
  adapt_images <- FALSE
  width <- 200
  height <- 100
  question <- "What is the square root of 16?"
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "Square Root Image"
  type <- "essay"
  author <- "John Doe"
  fb_general <- ""
  idnumber <- "Q003"

  # Call the function
  result <- xml_questiontext(copyright, license, adapt_images, width, height,
                             question, image, image_alt, type, author,
                             fb_general, idnumber)

  # Expectations
  expect_true(grepl("<generalfeedback format=\"html\">", result))
  expect_match(result, "<penalty>0</penalty>", fixed = TRUE) # Essay penalty is 0
})

test_that("xml_questiontext works with no image", {
  # Inputs
  copyright <- "Copyright Info"
  license <- "License Info"
  adapt_images <- FALSE
  width <- 200
  height <- 100
  question <- "What is the speed of light?"
  image <- ""
  image_alt <- ""
  type <- "truefalse"
  author <- "Albert Einstein"
  fb_general <- "Light speed is approximately 299,792 km/s."
  idnumber <- "Q004"

  # Call the function
  result <- xml_questiontext(copyright, license, adapt_images, width, height,
                             question, image, image_alt, type, author,
                             fb_general, idnumber)

  # Expectations
  expect_false(grepl("<p><img", result)) # No image tag should be included
  expect_match(result, "<p>What is the speed of light?</p>", fixed = TRUE)
})


test_that("xml_question_name generates correct XML for a valid name", {
  # Input
  name <- "Sample Question Name"

  # Call the function
  result <- xml_question_name(name)

  # Expectations
  expect_match(result, "<name>", fixed = TRUE)
  expect_match(result, "<text>Sample Question Name</text>", fixed = TRUE)
  expect_match(result, "</name>", fixed = TRUE)
})

test_that("xml_question_name handles empty name gracefully", {
  # Input
  name <- ""

  # Call the function
  result <- xml_question_name(name)

  # Expectations
  expect_match(result, "<name>", fixed = TRUE)
  expect_match(result, "<text></text>", fixed = TRUE) # Verifica que el nombre está vacío
  expect_match(result, "</name>", fixed = TRUE)
})

test_that("xml_question_name handles special characters correctly", {
  # Input
  name <- "Question <Name> & \"Special\""

  # Call the function
  result <- xml_question_name(name)

  # Expectations
  expect_match(result, "<name>", fixed = TRUE)
  expect_match(result, "<text>Question <Name> & \"Special\"</text>", fixed = TRUE)
  expect_match(result, "</name>", fixed = TRUE)
})

test_that("xml_question_idnumber generates correct XML for a valid idnumber", {
  # Input
  idnumber <- "12345"

  # Call the function
  result <- xml_question_idnumber(idnumber)

  # Expectations
  expect_match(result, "<idnumber>", fixed = TRUE)
  expect_match(result, "<idnumber>12345</idnumber>", fixed = TRUE) # Verifica el valor del idnumber
  expect_match(result, "</idnumber>", fixed = TRUE)
})

test_that("xml_question_idnumber handles empty idnumber gracefully", {
  # Input
  idnumber <- ""

  # Call the function
  result <- xml_question_idnumber(idnumber)

  # Expectations
  expect_match(result, "<idnumber>", fixed = TRUE)
  expect_match(result, "<idnumber></idnumber>", fixed = TRUE)
  expect_match(result, "</idnumber>", fixed = TRUE)
})

test_that("xml_question_idnumber handles special characters correctly", {
  # Input
  idnumber <- "ID<123>&\"ABC\""

  # Call the function
  result <- xml_question_idnumber(idnumber)

  # Expectations
  expect_match(result, "<idnumber>", fixed = TRUE)
  expect_match(result, "<idnumber>ID<123>&\"ABC\"</idnumber>", fixed = TRUE)
  expect_match(result, "</idnumber>", fixed = TRUE)
})

test_that("format_questions returns an empty string for empty input", {
  questions <- list()
  result <- format_questions(questions)
  expect_equal(result, "")
})

test_that("format_questions handles NULL input gracefully", {
  questions <- NULL
  result <- format_questions(questions)
  expect_equal(result, "")
})

xml_question_category <- function(category) {
  glue::glue('<question type="category">
<category>
<text>{category}</text>
</category>
</question>')
}


test_that("category_question generates correct XML structure", {
  # Test data
  category <- "Example Category"
  questions <- "<question>Sample Question</question>"

  # Expected output
  expected <- structure(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n<question type=\"category\">\n  <category> <text>$course$/top/Example Category</text> </category>\n  <info format=\"html\"> <text></text> </info>\n  <idnumber></idnumber>\n</question><question>Sample Question</question>\n</quiz>",
    class = c("glue", "character")
  )

  # Compare the result with the expected output
  result <- category_question(category, questions)
  expect_equal(result, expected)
})

test_that("category_question handles empty questions", {
  # Test data
  category <- "Example Category"
  questions <- ""

  # Expected output
  expected <- structure(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n<question type=\"category\">\n  <category> <text>$course$/top/Example Category</text> </category>\n  <info format=\"html\"> <text></text> </info>\n  <idnumber></idnumber>\n</question>\n</quiz>",
    class = c("glue", "character")
  )

  # Compare the result with the expected output
  result <- category_question(category, questions)
  expect_equal(result, expected)
})

test_that("category_question handles special characters in category", {
  # Test data
  category <- "Category & Special <Characters>"
  questions <- "<question>Sample Question</question>"

  # Expected output
  expected <- structure(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n<question type=\"category\">\n  <category> <text>$course$/top/Category & Special <Characters></text> </category>\n  <info format=\"html\"> <text></text> </info>\n  <idnumber></idnumber>\n</question><question>Sample Question</question>\n</quiz>",
    class = c("glue", "character")
  )

  # Compare the result with the expected output
  result <- category_question(category, questions)
  expect_equal(result, expected)
})

test_that("category_question handles empty category", {
  # Test data
  category <- ""
  questions <- "<question>Sample Question</question>"

  # Expected output
  expected <- structure(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<quiz>\n<question type=\"category\">\n  <category> <text>$course$/top/</text> </category>\n  <info format=\"html\"> <text></text> </info>\n  <idnumber></idnumber>\n</question><question>Sample Question</question>\n</quiz>",
    class = c("glue", "character")
  )

  # Compare the result with the expected output
  result <- category_question(category, questions)
  expect_equal(result, expected)
})




# Mock functions for testing
format_questions <- function(questions) {
  paste0("<question>", questions, "</question>")
}

extended_format_questions <- function(qc) {
  paste0("<extended>", qc$category, "</extended>")
}

category_question <- function(category, questions) {
  glue::glue(
    '<?xml version="1.0" encoding="UTF-8"?>
<quiz>',
    xml_question_category(category),
    questions,
    '
</quiz>'
  )
}

xml_question_category <- function(category) {
  glue::glue('<question type="category">
<category>
<text>{category}</text>
</category>
</question>')
}

