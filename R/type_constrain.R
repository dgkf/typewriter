#' Apply type constraints over observed type parameter values
#'
#' @param paramvals A list of type parameters and their observed values
#' @param ... Type constraint expressions, from which constraint functions can
#'   be produced.
#'
#' @return Returns `NULL`, though this function is called primarily for its
#'   side-effects of issueing errors when type constraint checking fails.
#'
#' @family type-evaluation
#' @export
#'
type_constrain <- function(paramvals, ...) {
  # ensure that type checks get applied
  force(paramvals)

  # if no constraints are provided, take an off-ramp
  dots <- append(lapply(names(paramvals), as.name), substitute(...()))
  if (is.null(dots)) return(invisible(NULL))  # nothing to check

  # build constraints from constraint functions
  constraint_fns <- build_parameter_constraint_fns(dots)

  # apply identity type constraints
  for (type_var in names(constraint_fns$constrain_all)) {
    constraint_fn <- constraint_fns$constrain_all[[type_var]]
    if (!constraint_fn(paramvals[[type_var]])) {
      constraint <- as.name(type_var)
      throw_type_bounds_error(constraint)
    }
  }

  # apply functional type constraints
  for (constraint_fn in constraint_fns$constrain_each) {
    type_vars <- names(formals(constraint_fn))
    names(type_vars) <- type_vars
    type_vars[] <- lapply(paramvals[names(type_vars)], unlist)
    type_val_grid <- do.call(expand.grid, type_vars)

    # apply type constraint to all applicable combinations of trait values
    type_val_grid_satisfied <- apply(type_val_grid, 1L, function(vals) {
      do.call(constraint_fn, as.list(vals))
    })

    if (!all(type_val_grid_satisfied)) {
      constraint <- body(constraint_fn)
      throw_type_bounds_error(constraint)
    }
  }

  invisible(NULL)
}
