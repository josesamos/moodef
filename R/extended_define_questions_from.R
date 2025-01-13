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

  qc$questions <- df
  qc$extended <- TRUE

  qc
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

    allowed_types <- c("numerical", "multichoice", "ordering", "ddwtos",
                       "gapselect", "matching", "essay", "truefalse",
                       "shortanswer", "ddmarker")
    all_valid <- all(df$type %in% allowed_types)

    if (!all_valid) {
      invalid_values <- unique(df$type[!df$type %in% allowed_types])
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
    fq <- glue::glue(fq, xml_question_category(category))

    questiontext <- xml_questiontext(
      qc$copyright,
      qc$license,
      qc$adapt_images,
      qc$width,
      qc$height,
      qc$questions[i, "question"],
      qc$questions[i, "image"],
      qc$questions[i, "image_alt"],
      qc$questions[i, "author"]
    )

    name <- xml_question_name(qc$questions[i, "name"])

    idnumber <- xml_question_idnumber(qc$questions[i, "id"])

    type <- qc$questions[["type"]][i]

  }

  fq <- glue::glue(fq, '
</quiz>
')
  fq
}
