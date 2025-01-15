test_that("generate_essay generates the correct default XML structure", {
  result <- generate_essay()

  # Expected output
  expected_structure <- structure(
    "\n<responseformat>editor</responseformat>\n<responserequired>1</responserequired>\n<responsefieldlines>10</responsefieldlines>\n<minwordlimit></minwordlimit>\n<maxwordlimit></maxwordlimit>\n<attachments>0</attachments>\n<attachmentsrequired>0</attachmentsrequired>\n<maxbytes>0</maxbytes>\n<filetypeslist></filetypeslist>\n<graderinfo format=\"html\">\n  <text></text>\n</graderinfo>\n<responsetemplate format=\"html\">\n  <text></text>\n</responsetemplate>",
    class = c("glue", "character")
  )

  # Check if the result matches the expected structure
  expect_equal(result, expected_structure)
})
