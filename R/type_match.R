#' Type check against type definition type
#'
#' Type checking relies on a series of method dispatch to evaluate different
#' syntactic types of type definitions. See S3 implementations for details.
#'
#' @section Syntax:
#' Type signatures do not follow typical R syntax.
#'
#' \describe{
#'   \item{`"t"`, `t`:}{
#'     Attempts to apply `t` as an interface predicate, and otherwise
#'     interprets`t` as a class. Similar to:
#'     \preformatted{
#'     # if a predicate function named "t" exists ("is.numeric")
#'     isTRUE(t(object))
#'
#'     # if no function named "t" exists ("numeric")
#'     t <- as.character(t)
#'     inherts(object, t) || mode(object) == t
#'     }
#'   }
#'   \item{`t1 | t2`:}{
#'     Union. Object is satisfied by `t1` or `t2`.
#'   }
#'   \item{`t(interface)`:}{
#'     Further constrains `t` by additional interface predicate.
#'   }
#'   \item{`t(interface=result)`:}{
#'     Further constrains `t` by an interface, whose result is constrained such
#'     that it must produce `result`. Can be interpretted as:
#'     \preformatted{
#'     type_match(t, object)
#'     interface(object) == result
#'     }
#'   }
#'   \item{`t(interface :type)`:}{
#'     Further constrains `t` by an interface, whose result is constrained such
#'     that it must be of type `type`. Can be interpretted as:
#'     \preformatted{
#'     type_match(t, object)
#'     type_match(type, interface(object))
#'     }
#'   }
#'   \item{`t[[index :type]]`:}{
#'     Further constrains `t` such that the element at `index` is constrained by
#'     type `type`. Can be interetted as:
#'     \preformatted{
#'     type_match(t, object)
#'     type_match(type, t[[index])
#'     }
#'   }
#'   \item{`t[elem_type]`:}{
#'     Further constrains `t` such that all elements are constrained by
#'     `slice_type`. Can be interetted as:
#'     \preformatted{
#'     type_match(t, object)
#'     all(vapply(t, type_match, logical(1L), elem_type))
#'     }
#'   }
#'   \item{`t[slice :elem_type]`:}{
#'     Further constrains `t` such that all elements of `object[slice]` are
#'     constrained by `elem_type`. Can be interpretted as:
#'     \preformatted{
#'     type_match(t, object)
#'     all(vapply(object[slice], type_match, logical(1L), elem_type))
#'     }
#'   }
#'   \item{`t(.$elem :elem_type)` or `t(elem$elem_type)`:}{
#'     Infix operators can be applied as dot-style lambdas to define a interface
#'     predicate, or the infix operator itself can be used as a shorthand
#'     notation. Can be interpretted as:
#'     \preformatted{
#'     type_match(t, object)
#'     type_match(elem_type, object$elem)
#'     }
#'   }
#' }
#'
#' @param type A type to type check against. On the surface, this is a
#' language object. The meaning of that language object is processed through a
#' chain of method dispatch.
#' @param name The name of the parameter to type check
#' @param val The value of the parameter to type check
#' @param envir The function environment where `name` is defined
#' @param paramvals An environment of observed type parameter values. This
#'   environment can be further processed to do type parameter bounds checks.
#'
#' @return A `logical` value indicating whether a type specification is met by
#' the value in the function environment. This function is also called for the
#' side-effect of mutating the `paramvals` environment to collect observed type
#' parameter values.
#'
#' @examples
#' # match type of object `c(1, 2)` against definition `"numeric"`
#' # can also be tested using %is% (below)
#' type_match("numeric", c(1, 2))
#'
#' # character class name
#' c(1, 2)      %is% "numeric"
#' c("a", "b")  %is% "numeric" # FALSE
#'
#' # name (interpretted as class name)
#' c(1, 2)      %is%  numeric
#' c("a", "b")  %is%  numeric  # FALSE
#'
#' # interface predicate
#' c(1, 2)      %is%  is.numeric
#' c("a", "b")  %is%  is.numeric
#'
#' # name type, qualified by interface predicate
#' 1L  %is%  numeric(is.finite)
#' Inf %is%  numeric(is.finite)  # FALSE
#'
#' # name type, qualified by interface return value
#' list(1, 2)     %is%  list(length=2)
#' list(1, 2, 3)  %is%  list(length=2)  # FALSE
#'
#' # name type, qualified by types by index
#' list(a = 1, b = "2")  %is%  list[[a :numeric, b :character]]
#' list(a = 1, b = 2)    %is%  list[[a :numeric, b :character]]  # FALSE
#'
#' # name type, qualified by element name type
#' list(1, 2, 3)    %is%  list[numeric]
#' list(1, 2, "c")  %is%  list[numeric]  # FALSE
#'
#' # name type, qualified by infix behavior
#' list(a = 1, b = "b")  %is%  list(.$a :numeric, .$b :character)
#' list(a = 1, b = "b")  %is%  list(a$numeric, b$character)
#'
#' # some composite examples
#' list(1, 2, 3)    %is%  list(length=3)[numeric]
#' list(1, 2)       %is%  list(length=3)[numeric]  # FALSE
#' list(1, 2, "c")  %is%  list(length=3)[numeric]  # FALSE
#'
#'
#' list(1:3, 1:3)  %is%  list(length=2)[numeric(length=3)]
#' list(1:3, 1)    %is%  list(length=2)[[1 :numeric(length=3), 2 :numeric]]
#'
#' iris  %is%  data.frame(.$Sepal.Length :numeric, .$Species :factor)
#' iris  %is%  data.frame[1:4 :numeric(length=150), "Species" :factor]
#' iris  %is%  data.frame(nrow=150)[1:4 :numeric, "Species" :factor]
#'
#' @export
#' @rdname type_match
#' @family type-evaluation
#'
type_match <- function(type, val, ..., quoted = FALSE) {
  if (!quoted) type <- substitute(type)
  type_match_(type, val, ...)
}



#' What is a type, but a miserable pile of secrets?
#'
#' The existential question of object types in a dynamically typed language.
#' This line of "type" is a bit arbitrary. There are plenty of dimensions that
#' could be considered as other gating criteria for type identity.
#'
#' You may prefer that objects of the same type always have attributes of the
#' same types, or attributes that are entirely identical. For the sake of
#' documenting some edge cases for consideration:
#'
#' @section Motivating Cases:
#'
#' `factor` variables might be considered identical only if they have the same
#' levels, akin to an enum. In this sense, the value of the `"label"` attribute
#' should be identical.
#'
#' However, if attribute values must be identical, then likewise any named
#' object would need to have identical names to match.
#'
#' Even if attributes just require the same type, then even `data.frame`s would
#' not be homogenous types, since `row.names` may be `numeric` or `character`.
#'
#' With that in mind, the definition of a type has been left a bit loose.
#' Without a stronger type system to support it, anything more exhaustive would
#' be overbearing.
#'
typeify <- function(obj) {
  list(.class = class(obj), .mode = mode(obj))
}



type_match_log_call <- function(type, debug = FALSE) {
  if (debug) cat(
    as.character(type),
    paste0("(", gsub(".*\\.", "", as.character(sys.call(-1L)[[1L]])), ")"),
    "\n"
  )
}



type_match_ <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  UseMethod("type_match_")
}



#' @exportS3Method
type_match_.default <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  throw_type_check_error(type, name)
}



#' @exportS3Method
type_match_.character <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  type_match_log_call(type, debug)

  if (type %in% names(paramvals)) {
    paramvals[[type]][[name]] <- typeify(val)
    return(TRUE)
  }

  mode(val) == type || inherits(val, type)
}



#' @exportS3Method
type_match_.name <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  type_match_log_call(type, debug)

  type_chr <- as.character(type)
  type_val <- mget(type_chr, envir, inherits = TRUE)[[1L]]
  if (missing(type_val)) {
    return(type_match_(type_chr, val, name, envir, paramvals, debug))
  }

  res <- suppressWarnings(tryCatch(
    do.call(type_chr, list(val), envir = envir),
    error = function(e) NULL
  ))

  if (is.logical(res) && length(res) == 1L) {
    return(res)
  }

  type_match_(type_chr, val, name, envir, paramvals, debug)
}



#' @exportS3Method type_match_ "function"
type_match_.function <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  type_match_log_call("function", debug)
  type(val)
}



#' @exportS3Method
type_match_.call <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  type_match_log_call(type[[1L]], debug)

  switch(as.character(type[[1L]]),
    "function" = {
      fn <- eval(type, envir = envir)
      return(type_match_(fn, val, name, envir, paramvals, debug))
    },
    "|" = return(type_match_union(type, val, name, envir, paramvals, debug)),
    "[" = return(type_match_slice(type, val, name, envir, paramvals, debug)),
    "[[" = return(type_match_elem(type, val, name, envir, paramvals, debug)),
    "$" = return(type_match_dollar(type, val, name, envir, paramvals, debug)),
    "@" = return(type_match_atsign(type, val, name, envir, paramvals, debug)),
    ":" = return(type_match_annotated(type, val, name, envir, paramvals, debug))
  )

  if (!type_match_(type[[1L]], val, name, envir, paramvals, debug)) {
    return(FALSE)
  }

  for (trait_i in seq_along(type)[-1L]) {
    trait_fn <- names(type)[[trait_i]]
    is_trait <- !(is.null(trait_fn) || nchar(trait_fn) == 0L)

    # if not a named key=value pair, then treat this as a contextual type bound
    if (!is_trait) {
      satisfied <- type_match_(type[[trait_i]], val, name, envir, paramvals, debug)
      if (satisfied) next
      else return(FALSE)
    }

    trait_actual <- do.call(trait_fn, list(val), envir = envir)
    trait_bound_chr <- as.character(type[[trait_i]])
    if (trait_bound_chr %in% names(paramvals)) {
      paramvals[[trait_bound_chr]][[name]] <- trait_actual
    } else {
      trait_bound <- eval(type[[trait_i]], envir = envir)
      if (is.atomic(trait_bound)) {
        # first try `==` operator, if that errors (for object comparisons),
        # try `identical`
        trait_is_valid <- tryCatch(
          trait_actual == trait_bound,
          error = function(e) identical(trait_actual, trait_bound)
        )

        if (!trait_is_valid) return(FALSE)
      }
    }
  }

  TRUE
}



type_match_union <- function(
  union_expr,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  union_checks <- vapply(
    union_expr[-1L],
    type_match_,
    logical(1L),
    val, name, envir, paramvals, debug
  )

  return(any(union_checks))
}



type_match_slice <- function(
  slice_expr,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  obj_check <- type_match_(slice_expr[[2L]], val, name, envir, paramvals, debug)
  if (!obj_check) return(FALSE)

  for (i in seq_along(slice_expr)[-(1:2)]) {
    elem_type <- slice_expr[[i]]

    # handle type annotated slices `[slice :type]`
    if (is.call(elem_type) && elem_type[[1L]] == ":") {
      elem_slice <- eval(elem_type[[2L]], envir)
      elem_slice_name <- sprintf("%s[%s]", name, as.character(elem_slice))

      elem_checks <- vapply(
        do.call("[", list(val, elem_slice), envir = envir),
        type_match_,
        logical(1L),
        type = elem_type[[3L]],
        name = elem_slice_name, envir, paramvals, debug
      )

      if (!all(elem_checks)) return(FALSE)

    # handle all element annotations `[type]`
    } else {
      elem_slice_name <- sprintf("%s[.]", name)

      elem_checks <- vapply(
        val,
        type_match_,
        logical(1L),
        type = slice_expr[[3L]],
        name = elem_slice_name, envir, paramvals, debug
      )

      if (!all(elem_checks)) return(FALSE)
    }
  }

  TRUE
}



type_match_elem <- function(
  elem_expr,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  obj_check <- type_match_(elem_expr[[2L]], val, name, envir, paramvals, debug)
  if (!obj_check) return(FALSE)

  for (i in seq_along(elem_expr)[-(1:2)]) {
    elem_type <- elem_expr[[i]]

    # handle type annotated index types `[[index :type]]`
    if (is.call(elem_type) && elem_type[[1L]] == ":") {

      elem_val <- tryCatch({
        do.call("[[", list(val, elem_type[[2L]]), envir = envir)
      }, error = function(e) {
        do.call("[[", list(val, as.character(elem_type[[2L]])), envir = envir)
      })

      elem_name <- sprintf("%s[[%s]]", name, as.character(elem_type[[2L]]))
      elem_check <- type_match_(
        elem_type[[3L]],
        elem_val,
        name = elem_name,
        envir, paramvals, debug
      )

      if (!elem_check) return(FALSE)

    # handle all element annotations `[[type]]`
    } else {

      elem_name <- sprintf("%s[[%s]]", name, as.character(elem_expr[[3L]]))
      elem_checks <- vapply(
        val,
        type_match_,
        logical(1L),
        type = elem_expr[[3L]],
        name = elem_name,
        envir, paramvals, debug
      )

      if (!all(elem_checks)) return(FALSE)
    }
  }

  TRUE
}



type_match_dollar <- function(
  dollar_expr,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  elem_dollar_name <- sprintf("%s$%s", name, as.character(dollar_expr[[2L]]))
  type_match_(
    dollar_expr[[3L]],
    do.call("$", list(val, dollar_expr[[2L]]), envir = envir),
    name = elem_dollar_name, envir, paramvals, debug
  )
}




type_match_atsign <- function(
  atsign_expr,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  elem_atsign_name <- sprintf("%s@%s", name, as.character(atsign_expr[[2L]]))
  type_match_(
    atsign_expr[[3L]],
    `@`(val, as.character(atsign_expr[[2L]])),
    name = elem_atsign_name, envir, paramvals, debug
  )
}



type_match_annotated <- function(
  type,
  val,
  name,
  envir = parent.frame(),
  paramvals = emptyenv(),
  debug = FALSE
) {
  if (is.name(type[[2L]])) {
    type_match_(
      type[[3L]],
      do.call(as.character(type[[2L]]), list(val), envir = envir),
      name, envir, paramvals, debug
    )
  } else {
    type_match_(
      type[[3L]],
      eval(do.call(substitute, list(type[[2L]], list("." = val))), envir = envir),
      name, envir, paramvals, debug
    )
  }
}
