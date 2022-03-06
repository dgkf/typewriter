#' Split type definitions from parameter defaults
#'
#' In order to use the `:` operator as a delimiter for type annotations, some
#' syntax-tree rearrangement must happen to account for this operator's high
#' precedence. This function takes an expression and splits it based on the last
#' surface-level `:` not followed by a numeric atomic value.
#'
#' @note This assumes that the `:` is only implemented for numeric ranges. For
#' now, that is a safe assumption, but if a package implements a character or
#' symbol range, this assumption may result in improper interpretation of valid
#' parameter defaults as type definitions.
#'
#' @param expr An expression to split based on last `:`
#' @param depth Recursion depth (not expected to be passed by a user)
#'
#' @return A list with named fields `value` and `type`, containing the default
#' value specification and type split from the provided definition. If a value
#' is equal to `.` it is considered as a placeholder and returns an empty list.
#'
#' @examples
#' split_arg_type_checking(quote("default" :character))
#' split_arg_type_checking(quote(1:3 :numeric(length=3)))
#' split_arg_type_checking(quote(list(1, 1:2, 1:3) :list(lengths=1:3)))
#' split_arg_type_checking(quote(. :character))
#'
#' @family type_parsing
#'
split_arg_type_checking <- function(expr, depth = 0L) {
  arg <- list(value = list(), type = list())

  if (length(expr) == 1L && as.character(expr) == "") return(arg)

  if (length(expr) == 1L) {
    arg$value <- expr
  } else if (length(expr) > 1L && expr[[1L]] == quote(`:`) &&
    !(is.atomic(expr[[3L]]) && class(expr[[3L]]) == "numeric")) {
    arg$value <- expr[[2L]]
    arg$type <- expr[[3L]]
  } else {
    expr <- as.list(expr)
    for (i in rev(seq_along(expr))) {
      s <- split_arg_type_checking(expr[[i]], depth + 1L)
      if (length(s$value) > 1L) s$value <- as.call(s$value)
      if (length(s$type) > 0L) {
        if (i == length(expr)) {
          arg$value <- append(head(expr, i - 1L), s$value)
          arg$type  <- s$type
        } else {
          arg$value <- append(head(expr, i - 1L)[-1L], s$value)
          arg$type  <- c(expr[1L], s$type, tail(expr, -i))
        }
        break
      } else if (i == length(expr)) {
        arg$value <- expr
      }
    }
  }

  if (depth == 0L && length(arg$value) > 1L) {
    arg$value <- as.call(arg$value)
  } else if (length(arg$value) == 1L && arg$value == quote(`.`)) {
    arg$value <- list()
  }

  if (depth == 0L && length(arg$type) > 1L) {
    arg$type <- as.call(arg$type)
  }

  arg
}



#' Split arguments to extract type information
#'
#' @param fn A function definition from which to extract parameter information
#'
#' @return A list of split type information as provided by
#' \code{\link{split_arg_type_checking}}
#'
#' @family type_parsing
#'
split_fn_type_checking <- function(fn) {
  fn_formals <- formals(fn)
  lapply(fn_formals, split_arg_type_checking)
}
