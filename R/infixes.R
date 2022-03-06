#' @export
#' @rdname type_match
`%is%` <- function(val, type) {
  type <- substitute(type)
  if (length(type) == 2L && type[[1L]] == "(")
    type <- type[[2L]]
  type_match(type, val, quoted = TRUE)
}

#' @export
`%>%` <- function(lhs, rhs, quoted = FALSE, envir = parent.frame()) {
  if (!quoted) {
    rhsexpr <- substitute(rhs)
    lhsexpr <- substitute(lhs)
  } else {
    rhsexpr <- rhs
    lhsexpr <- lhs
  }

  if (is.name(rhsexpr)) {
    rhsexpr <- call(as.character(rhsexpr))
  } else if (rhsexpr[[1L]] == "function") {
    rhsexpr <- bquote((.(rhsexpr))())
  }

  rhsfn <- as.character(rhsexpr[[1L]])
  needs_eval <- rhsfn %in% c("(", "{") || !grepl("^[._A-z]", rhsfn)
  if (needs_eval) {
    rhs <- eval(rhs, envir = envir)
    return(rhs(lhs))
  }

  rhsexpr <- as.call(append(
    as.list(rhsexpr),
    list(lhsexpr),
    after = 1L
  ))

  eval(rhsexpr, envir = envir)
}

#' @export
`%<%` <- function(lhs, rhs, quoted = FALSE, envir = parent.frame()) {
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  `%>%`(rhs, lhs, quoted = TRUE, envir = envir)
}

#' @export
`%@%` <- `%<%`
