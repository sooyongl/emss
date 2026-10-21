#' #' S3 method
#' #'
#' #' @export print.ess
#' #' @export
#' print.ess <- function(x) {
#'     print(list(
#'       ess_table = head(x$ess_table),
#'       review    = head(x$review)
#'     ))
#' }
