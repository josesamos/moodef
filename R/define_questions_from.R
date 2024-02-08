

define_questions_from_csv <- function(qc, file, sep = ',') {
  table <- readr::read_delim(
    file,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character())
  )
  attributes <- names(table)
  table[, attributes] <-
    apply(table[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ''))
  attributes <- snakecase::to_snake_case(attributes)
  names(table) <- attributes
  for (opcional in c('type', 'image', 'image_alt')) {
    if (!(opcional %in% attributes)) {
      table[, opcional] <- ''
    }
  }
  rest <- setdiff(attributes, c("type", "question", "image", "image_alt", "answer"))
  for (i in 1:nrow(table)) {
    text <- paste0(
      "define_question(qc, type = '",
      table[i, 'type'],
      "', question = '",
      table[i, 'question'],
      "', image = '",
      table[i, 'image'],
      "', image_alt = '",
      table[i, 'image_alt'],
      "', answer = ",
      string_to_string_vector(table[i, 'answer'][[1]])
    )
    j <- 0
    for (r in rest) {
      if (table[i, r][[1]] != '') {
        j <- j + 1
        text <- paste0(text,
                       ", a_", j, " = ", string_to_string_vector(table[i, r][[1]]))
      }
    }
    text <- paste0(text, ")")
    qc <- eval(parse(text = text))
  }
  qc
}
