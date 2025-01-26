test_that("generate_ddmarker generates correct XML structure with single answer", {
  # Inputs
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "An example image"
  answer <- c("circle", "10,20,30", "Answer text")
  fb_answer <- character(0)  # No feedback answers

  expected_structure <- structure(
    "\n\n<drag>\n  <no>1</no>\n  <text>Answer text</text>\n  <noofdrags>1</noofdrags>\n</drag>\n<drop>\n  <no>1</no>\n  <shape>circle</shape>\n  <coords>10,20,30</coords>\n  <choice>1</choice>\n</drop>",
    class = c("glue", "character")
  )

  # Run the function
  result <- generate_ddmarker(
    image = image,
    image_alt = image_alt,
    answer = answer,
    fb_answer = fb_answer
  )

  result <- gsub("<file.*?</file>", "", result)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})

test_that("generate_ddmarker generates correct XML structure with multiple feedback answers", {
  # Inputs
  image <- system.file("extdata", "divide.png", package = "moodef")
  image_alt <- "An example image"
  answer <- c("circle", "10,20,30", "Correct Answer")
  fb_answer <- c("rectangle<|>40,50;60<|>Feedback 1", "triangle<|>70,80;90<|>Feedback 2")

  # Expected structure
  expected_structure <- structure(
    "\n<drag>\n  <no>1</no>\n  <text>Correct Answer</text>\n  <noofdrags>1</noofdrags>\n</drag>\n<drop>\n  <no>1</no>\n  <shape>circle</shape>\n  <coords>10,20,30</coords>\n  <choice>1</choice>\n</drop>\n\n    <drag>\n      <no>2</no>\n      <text>Feedback 1</text>\n      <noofdrags>2</noofdrags>\n    </drag>\n    <drop>\n      <no>2</no>\n      <shape>rectangle</shape>\n      <coords>40,50;60</coords>\n      <choice>2</choice>\n    </drop>\n\n    <drag>\n      <no>3</no>\n      <text>Feedback 2</text>\n      <noofdrags>3</noofdrags>\n    </drag>\n    <drop>\n      <no>3</no>\n      <shape>triangle</shape>\n      <coords>70,80;90</coords>\n      <choice>3</choice>\n    </drop>",
    class = c("glue", "character")
  )

  # Run the function
  result <- generate_ddmarker(
    image = image,
    image_alt = image_alt,
    answer = answer,
    fb_answer = fb_answer
  )

  result <- gsub("<file.*?</file>", "", result)

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
