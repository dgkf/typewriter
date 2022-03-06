#' Post-process a function defintion to apply type constraints
#'
#' This function interprets the syntactic typing sugar and produces a standard R
#' function, stripping type annotations from the function header and embedding
#' them as a call to `type_check` as the first expression in the function body.
#'
#' @param fn A function to post-process
#'
#' @return `fn` with type annotations stripped from the function header and
#' passed instead to a call to `type_check`.
#'
#' @examples
#' fn <- type(function(a = 1 :numeric) {
#'   print(a + 2)
#' })
#'
#' fn
#' #> function (a = 1) {
#' #>   type_constrain(type_check(character(0), a = numeric))
#' #>   print(a + 2)
#' #> }
#'
#' fn(3)
#' #> [1] 5
#'
#' fn("test")
#' #> Error in throw_type_check_error(signature, name) :
#' #>   Type of parameter 'a' does not match signature `numeric`
#'
#' @export
#'
type <- function(fn, ...) {
  constraints <- substitute(...())
  arg_data <- split_fn_type_checking(fn)

  type_check_args <- Filter(length, lapply(arg_data, `[[`, "type"))

  type_params = constraints[which_type_parameters(constraints)]
  type_check_call <- do.call("call", quote = TRUE, c(
    "type_check",
    list(type_params = as.character(type_params)),
    type_check_args
  ))

  type_constrain_call <- as.call(append(
    as.list(call("type_constrain", type_check_call)),
    strip_type_parameters(constraints)
  ))

  # strip out typing information from header
  formals(fn) <- lapply(arg_data, function(i) {
    if (length(i$value)) i$value else missing_value()
  })

  # insert type checking statements
  if (body(fn)[[1L]] == quote(`{`)) {
    body(fn) <- as.call(append(
      as.list(body(fn)),
      after = 1L,
      list(type_constrain_call)
    ))
  } else {
    body(fn) <- as.call(list(
      quote(`{`),
      type_constrain_call,
      body(fn)
    ))
  }

  fn
}
