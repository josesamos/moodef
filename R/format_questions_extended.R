utils::globalVariables(
  c(
    "category",
    "author",
    "fb_general",
    "fb_correct",
    "fb_partially",
    "fb_incorrect",
    "id",
    "duplicate_ids"
  )
)

#' Define extended questions from a data frame
#'
#' @keywords internal
define_extended_questions_from_data_frame <- function(qc, df) {

  df <- validate_and_adjust_dataframe(df)
  invalid_rows <- which((df$image != "" & df$image_alt == "") | (df$image == "" & df$image_alt != ""))
  if (length(invalid_rows) > 0) {
    message("Validation failed at rows: ", paste(invalid_rows, collapse = ", "))
    stopifnot('If an image is included, the associated alt field must also be defined.' = length(invalid_rows) == 0)
  }

  df <- df |>
    dplyr::mutate(category = dplyr::if_else(category == "", qc$category, category)) |>
    dplyr::mutate(author = dplyr::if_else(author == "", qc$author, author)) |>
    dplyr::mutate(fb_general = dplyr::if_else(fb_general == "", qc$general_feedback, fb_general)) |>
    dplyr::mutate(fb_correct = dplyr::if_else(fb_correct == "", qc$correct_feedback, fb_correct)) |>
    dplyr::mutate(fb_partially = dplyr::if_else(fb_partially == "", qc$partially_correct_feedback, fb_partially)) |>
    dplyr::mutate(fb_incorrect = dplyr::if_else(fb_incorrect == "", qc$incorrect_feedback, fb_incorrect))

  # check duplicate id values within category
  filtered_df <- df[df$id != "", ]
  duplicates <- filtered_df |>
    dplyr::group_by(category) |>
    dplyr::summarise(duplicate_ids = list(id[duplicated(id)]), .groups = "drop") |>
    dplyr::filter(lengths(duplicate_ids) > 0)
  if (nrow(duplicates) > 0) {
    warning(paste0(
      "Duplicate 'id' values found for the following 'category' values: ",
      paste(duplicates, collapse = ", ")
    ))
  }

  df$type <- get_detailed_type_names(df)

  qc$questions <- df
  qc$extended <- TRUE

  qc
}


#' Get Detailed Type Names
#'
#' Determines the `type` column of a data frame based on specific conditions and
#' the content of other columns in the data frame.
#'
#' It checks if all the values in the `type` column are within the allowed set
#' (`'', 'h', 'v', 'x'`) and updates each row based on the associated answers
#' and other fields.
#'
#' @param df A data frame containing at least the following columns:
#'   - `type`: A character column representing the type of each question.
#'   - `answer`: A character column containing answers for each question.
#'   - Additional columns with a prefix "a_" representing associated answer fields.
#'
#' @return A character vector containing the updated `type` values.
#' @keywords internal
get_detailed_type_names <- function(df) {
  is_simplified_type <- df$type %in% c('', 'h', 'v', 'x')
  if (any(is_simplified_type)) {
    for (i in 1:nrow(df)) {
      if (is_simplified_type[i]) {
        answer <- get_vector_answer(df$answer[i])
        a_values <- get_non_empty_fields_by_prefix(df, i, "a_")
        df$type[i] <- determine_question_type(df$type[i], df$question[i], answer, a_values)
      }
    }
  }
  df$type
}


#' Validate and Adjust a Dataframe Structure
#'
#' This function validates the structure of a dataframe to ensure it adheres to the expected format.
#' Specifically, it checks for the presence of required columns, verifies that the columns following
#' "answer" and "fb_answer" have a consistent structure, and renames columns as needed to match
#' the expected naming convention. If errors are detected, they are reported as warnings.
#'
#' @param df A dataframe to validate and adjust. The dataframe must have column names.
#'
#' @return If the dataframe structure is valid, it returns the dataframe with updated column names
#' for the sections following "answer", "fb_answer", and "tag_1". If there are validation errors,
#' the function issues warnings and returns the input dataframe unchanged.
#'
#' @details
#' The function performs the following checks:
#' 1. Ensures all required columns up to `"answer"` are present.
#' 2. Verifies that the column names between `"answer"` and `"fb_answer"`, and between `"fb_answer"`
#'    and `"tag_1"`, have the same count.
#' 3. Renames the columns after `"answer"` (e.g., `"a_1"`, `"a_2"`), `"fb_answer"` (e.g., `"fb_a_1"`,
#'    `"fb_a_2"`), and `"tag_1"` (e.g., `"tag_1"`, `"tag_2"`) based on their respective positions.
#'
#' @keywords internal
validate_and_adjust_dataframe <- function(df) {
  desired_order <- c(
    "category",
    "type",
    "id",
    "name",
    "author",
    "fb_general",
    "fb_correct",
    "fb_partially",
    "fb_incorrect",
    "question",
    "image",
    "image_alt",
    "answer"
  )
  required_columns <- c(
    desired_order,
    "fb_answer",
    "tag_1"
  )

  errors <- c()  # Initialize a list to store errors

  # Check if required columns up to 'answer' are present
  missing_cols <- setdiff(required_columns, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  remaining_columns <- setdiff(names(df), desired_order)
  final_order <- c(desired_order, remaining_columns)
  df <- df[, final_order, drop = FALSE]

  # Identify key column positions
  answer_idx <- which(names(df) == "answer")
  fb_answer_idx <- which(names(df) == "fb_answer")
  tag_1_idx <- which(names(df) == "tag_1")

  # Validate column structure only if critical columns are present
  if (length(errors) == 0) {
    # Calculate number of columns between "answer" and "fb_answer"
    cols_after_answer <- names(df)[(answer_idx + 1):(fb_answer_idx - 1)]
    cols_after_fb_answer <- names(df)[(fb_answer_idx + 1):(tag_1_idx - 1)]

    # Check if the number of columns matches
    if (length(cols_after_answer) != length(cols_after_fb_answer)) {
      errors <- c(errors, paste0(
        "Mismatch: ", length(cols_after_answer),
        " columns after 'answer' but ", length(cols_after_fb_answer),
        " columns after 'fb_answer'."
      ))
    }

    simplified_types <- c('', 'h', 'v', 'x')

    allowed_types <- c("numerical", "multichoice", "ordering", "ordering<|>h", "ordering<|>v", "ddwtos",
                       "gapselect", "matching", "essay", "truefalse",
                       "shortanswer", "ddmarker")
    all_valid <- all(df$type %in% c(allowed_types, simplified_types))

    if (!all_valid) {
      invalid_values <- unique(df$type[!df$type %in% c(allowed_types, simplified_types)])
      errors <- c(errors, paste0(
        "The 'type' column contains invalid values: ",
        paste(invalid_values, collapse = ", ")
      ))
    }

    # Rename columns if valid structure
    if (length(errors) == 0) {
      new_cols_after_answer <- paste0("a_", seq_along(cols_after_answer))
      new_cols_after_fb_answer <- paste0("fb_a_", seq_along(cols_after_fb_answer))
      new_cols_tags <- paste0("tag_", seq_len(ncol(df) - tag_1_idx + 1))

      # Rename columns
      names(df)[(answer_idx + 1):(fb_answer_idx - 1)] <- new_cols_after_answer
      names(df)[(fb_answer_idx + 1):(tag_1_idx - 1)] <- new_cols_after_fb_answer
      names(df)[tag_1_idx:ncol(df)] <- new_cols_tags

      return(df)
    }
  }

  # Return errors if any
  if (length(errors) > 0) {
    for (error in errors) {
      warning(error)
    }
  }

  return(df)
}


#' Get non-empty values from fields with a specific prefix
#'
#' This function takes a dataframe, a row index, and a prefix, returning a vector
#' with the content of columns that start with the given prefix and are not empty.
#'
#' @param df A dataframe containing the relevant columns.
#' @param i An integer representing the row index.
#' @param prefix A string representing the prefix of the column names (e.g., "a_", "fb_a_", "tag_").
#'
#' @return A vector with non-empty values from the fields with the given prefix in the specified row.
#' @keywords internal
get_non_empty_fields_by_prefix <- function(df, i, prefix) {
  # Ensure the index is within bounds
  if (i < 1 || i > nrow(df)) {
    stop("Row index is out of bounds.")
  }

  # Select columns that start with the given prefix
  matching_columns <- grep(paste0("^", prefix), names(df), value = TRUE)

  # Filter non-empty values in the specified row
  non_empty_values <- df[i, matching_columns, drop = FALSE] |>
    dplyr::select(dplyr::where(~ .x != "")) |>
    unlist(use.names = FALSE)

  return(non_empty_values)
}


#' Format all questions in the data frame
#'
#' @param qc A `question_category` object.
#'
#' @return A string.
#' @keywords internal
extended_format_questions <- function(qc) {
  fq <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
<quiz>
')

  for (i in 1:nrow(qc$questions)) {
    category <- qc$questions[i, "category"]
    question_category <- xml_question_category(category)

    type <- qc$questions[["type"]][i]
    # "ordering", "ordering<|>h", "ordering<|>v"
    type <- string_to_vector(type)
    if (is.na(type[2])) {
      orientation <- 'v'
    } else {
      orientation <- type[2]
      type <- type[1]
    }

    questiontext <- xml_questiontext(
      qc$copyright,
      qc$license,
      qc$adapt_images,
      qc$width,
      qc$height,
      qc$questions[["question"]][i],
      qc$questions[["image"]][i],
      qc$questions[["image_alt"]][i],
      type,
      qc$questions[["author"]][i],
      qc$questions[["fb_general"]][i],
      qc$questions[["id"]][i]
    )

    name <- xml_question_name(qc$questions[i, "name"])

    idnumber <- xml_question_idnumber(qc$questions[i, "id"])

    answer <- qc$questions[["answer"]][i]
    answer <- string_to_vector(answer)
    if (is.null(answer)) {
      answer <- ''
    }
    a_values <- get_non_empty_fields_by_prefix(qc$questions, i, "a_")
    fb_answer <- qc$questions[["fb_answer"]][i]
    fb_a_values <- get_non_empty_fields_by_prefix(qc$questions, i, "fb_a_")
    fb_correct <- qc$questions[["fb_correct"]][i]
    fb_incorrect <- qc$questions[["fb_incorrect"]][i]
    fb_partially <- qc$questions[["fb_partially"]][i]

    question_body <- switch(
      type,
      numerical = generate_numerical(answer, a_values, fb_answer, fb_a_values),
      multichoice = generate_multichoice(
        answer,
        a_values,
        fb_correct,
        fb_incorrect,
        fb_partially,
        fb_answer,
        fb_a_values
      ),
      ordering = generate_ordering(
        answer,
        a_values,
        fb_correct,
        fb_incorrect,
        fb_partially,
        orientation
      ),
      ddwtos = generate_ddwtos(answer, a_values, fb_correct, fb_incorrect, fb_partially),
      gapselect = generate_gapselect(answer, a_values, fb_correct, fb_incorrect, fb_partially),
      matching = generate_matching(answer, a_values, fb_correct, fb_incorrect, fb_partially),
      essay = generate_essay(),
      truefalse = generate_truefalse(answer, fb_answer, fb_a_values),
      shortanswer = generate_shortanswer(answer, fb_answer),
      ddmarker = generate_ddmarker(
        qc$questions[["image"]][i],
        qc$questions[["image_alt"]][i],
        answer,
        a_values
      ),
      warning(paste0("Unknown type: ", type))
    )

    tag_values <- get_non_empty_fields_by_prefix(qc$questions, i, "tag_")
    question_tags <- xml_question_tags(tag_values)

    question <- xml_question(type, name, questiontext, question_body, question_tags)
    fq <- glue::glue(fq, question_category, question)
  }

  fq <- glue::glue(fq, '
</quiz>
')
  fq
}
