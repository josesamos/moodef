
utils::globalVariables(c("simplified_types", "allowed_types"))

utils::globalVariables(
  c(
    "category",
    "penalty",
    "author",
    "fb_general",
    "fb_correct",
    "fb_partially",
    "fb_incorrect",
    "id",
    "duplicate_ids"
  )
)

#' Define questions from a data frame
#'
#' @keywords internal
define_questions_from_df <- function(qc, df) {

  invalid_rows <- which((df$image != "" & df$image_alt == "") | (df$image == "" & df$image_alt != ""))
  if (length(invalid_rows) > 0) {
    message("Validation failed at rows: ", paste(invalid_rows, collapse = ", "))
    stopifnot('If an image is included, the associated alt field must also be defined.' = length(invalid_rows) == 0)
  }

  df <- df |>
    dplyr::mutate(category = dplyr::if_else(category == "", qc$category, category)) |>
    dplyr::mutate(penalty = dplyr::if_else(penalty == "", qc$penalty, penalty)) |>
    dplyr::mutate(author = dplyr::if_else(author == "", qc$author, author)) |>
    dplyr::mutate(fb_correct = dplyr::if_else(fb_correct == "", qc$correct_feedback, fb_correct)) |>
    dplyr::mutate(fb_partially = dplyr::if_else(fb_partially == "", qc$partially_correct_feedback, fb_partially)) |>
    dplyr::mutate(fb_incorrect = dplyr::if_else(fb_incorrect == "", qc$incorrect_feedback, fb_incorrect))

  valid_penalty <- all(!is.na(as.numeric(df$penalty)) & as.numeric(df$penalty) >= 0 & as.numeric(df$penalty) <= 1)
  stopifnot('Penalty must be a number between 0 and 1.' = valid_penalty == TRUE)

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

  # define name of unnamed questions
  no_name <- which(df$name == "")
  for (i in no_name) {
    df$name[i] <- generate_question_name(qc$first_question_number, df$type[i], df$question[i])
    qc$first_question_number <- qc$first_question_number + 1
  }

  qc$questions <- df
  qc
}


#' Get Detailed Type Names
#'
#' Determines the `type` column of a data frame based on specific conditions and
#' the content of other columns in the data frame.
#'
#' It checks if some value in the `type` column are within the allowed set
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
  is_simplified_type <- df$type %in% simplified_types
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
