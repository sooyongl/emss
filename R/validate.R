#' @include ess_data.R
NULL

#' Check input data
#'
#' Kept for compatibility: runs the checks of [ess_data()] on a list of
#' data frames in the workbook's sheet order (setup, panelists, ratings, item
#' data, examinee data) and reports the result as text.
#'
#' @param imprt_data A list of data frames, or an object from [ess_data()].
#' @return `"Data input is validated"`, or the first problem found.
#' @export
validateData <- function(imprt_data) {
  tryCatch({
    as_ess_data(imprt_data)
    "Data input is validated"
  }, error = function(e) conditionMessage(e))
}
