#' A simple wrapper used for providing a missing value
#'
missing_value <- function(x) {
  stopifnot("Can't provide a missing value if x isn't missing" = missing(x))
  formals()[[1L]]
}



#' Execute code only if a variable is missing
#'
#' @param name A value to test for missingings
#' @param or An expression to evaluate if `name` is missing
#' @param envir The environment in which to check for `name`
#'
#' @return The value of `name` if it is not missing, or `NULL` otherwise.
#'    If `name` is missing, this function is used for the side-effects produced
#'    by executing `or`.
#'
is_not_missing_or <- function(name, or, envir = parent.frame()) {
  val <- mget(as.character(name), envir)[[1L]]
  if (missing(val)) force(or)
  else val
}
