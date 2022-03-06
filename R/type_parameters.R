#' Extract type parameters
#'
#' Genericly, this function simply extract single names from a list of
#' expressions, returned as a character vector.
#'
#' @param exprs A list of language objects
#' @return A character vector of names
#'
which_type_parameters <- function(exprs) {
  vapply(exprs, is.name, logical(1L))
}



strip_type_parameters <- function(exprs) {
  exprs[!which_type_parameters(exprs)]
}



#' Build type parameter constraints
#'
#' Provided a list of type parameters, build functions which can be applied over
#' runtime observed parameter values to test for constraint satisfaction.
#'
#' Type constraints are returned as one of two classes of functions,
#' `constrain_all` and `constrain_each`. Names produce `constrain_all`
#' functions, which expect that all observed type parameters are identical,
#' whereas expressions produce `constrain_each` functions, which expect that all
#' combinations of observed type parameters satisfy the constraint predicate.
#'
#' @param exprs A list of language objects representing type parameters and type
#'   parameter constraints
#'
#' @return A list with named fields `constrain_all` and `constrain_each`
#' representing two classes of constraints. `constrain_all` constraints expect
#' that all observed values have identical results. `constrain each` constraints
#' expect that each observed value satisfies the constraint predicate.
#'
#' @examples
#' build_parameter_constraint_fns(list(quote(T), quote(N), quote(N > 3)))
#'
build_parameter_constraint_fns <- function(exprs) {
  which_def <- which_type_parameters(exprs)
  type_defs <- as.character(exprs[which_def])

  fns_all <- exprs[which_def]
  names(fns_all) <- type_defs
  fns_all[] <- list(function(vals) {
    all(vapply(vals[-1], identical, logical(1L), vals[[1L]]))
  })

  fns_each <- list()
  for (i in seq_along(exprs)[!which_def]) {
    expr <- exprs[[i]]

    expr_fn <- function() { }
    expr_fn_formals <- intersect(all.names(expr), type_defs)
    names(expr_fn_formals) <- expr_fn_formals
    expr_fn_formals[] <- list(NULL)

    formals(expr_fn) <- expr_fn_formals
    body(expr_fn) <- expr

    fns_each[[length(fns_each)+1L]] <- expr_fn
  }

  list(constrain_all = fns_all, constrain_each = fns_each)
}
