#' Type check named arguments against type paramvalsraints
#'
#' @param type_defs A \code{character} vector of type parameters
#' @param ... Named type definition pairs. The name of the parameter represents
#'   the variable in the parent environment which should be type checked, and
#'   the value represents the type definition.
#'
#' @return A list of type bounds and their observed values over tested variables
#'
#' @family type-evaluation
#' @export
#'
type_check <- function(..., type_params) {
  args <- substitute(...())
  envir <- parent.frame()

  # initialize type paramvalsraints accumulator
  paramvals <- new.env(parent = emptyenv())
  for (t in type_params) paramvals[[t]] <- list()

  for (i in seq_along(args)) {
    signature <- args[[i]]
    name <- names(args)[[i]]
    val <- is_not_missing_or(name, next, envir)
    if (!type_match(signature, val, name, envir, paramvals, quoted = TRUE)) {
      throw_type_check_error(signature, name)
    }
  }

  as.list(paramvals)
}
