#' Simplified Question Types
#'
#' A vector of simplified question types used internally in the package.
#'
#' @format A character vector with 4 elements:
#' \describe{
#'   \item{\code{""}}{Empty type}
#'   \item{\code{"h"}}{Horizontal type}
#'   \item{\code{"v"}}{Vertical type}
#'   \item{\code{"x"}}{Other type}
#' }
#' @keywords internal
"simplified_types"

#' Allowed Question Types
#'
#' A vector of allowed question types for validation in the package.
#'
#' @format A character vector with 12 elements, including:
#' \describe{
#'   \item{\code{"numerical"}}{Numerical questions}
#'   \item{\code{"multichoice"}}{Multiple-choice questions}
#'   \item{\code{"ordering"}}{Ordering questions}
#'   \item{\code{"ordering<|>h"}}{Horizontal ordering questions}
#'   \item{\code{"ordering<|>v"}}{Vertical ordering questions}
#'   \item{\code{"ddwtos"}}{Drag-and-drop words into sentences}
#'   \item{\code{"gapselect"}}{Gap select questions}
#'   \item{\code{"matching"}}{Matching questions}
#'   \item{\code{"essay"}}{Essay questions}
#'   \item{\code{"truefalse"}}{True/False questions}
#'   \item{\code{"shortanswer"}}{Short-answer questions}
#'   \item{\code{"ddmarker"}}{Drag-and-drop markers}
#' }
#' @keywords internal
"allowed_types"
