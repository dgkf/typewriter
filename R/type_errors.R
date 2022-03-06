throw_type_check_error <- function(signature, name) {
  stop(errorCondition(class = c("TypeError"),
    sprintf(
      "Type of parameter '%s' does not match signature `%s`",
      name,
      deparse(signature)
    )
  ))
}

throw_type_bounds_error <- function(bounds) {
  stop(errorCondition(class = c("TypeParameterError", "TypeError"),
    sprintf("Type parameter '%s' not satisfied", deparse(bounds))
  ))
}
