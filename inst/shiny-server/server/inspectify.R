is.iterable <- function(x) {
  is.vector(x) || is.list(x) || is.matrix(x) || is.data.frame(x)
}

is.reactivevalues <- function(x) {
  inherits(x, "reactivevalues")
}

isTooLarge <- function(x) {
  object.size(x) > 1e6 # for example, 1MB
}

inspectify <- function(variable) {
  info_message <- deparse(substitute(variable))
  variable_type <- class(variable)
  call_string <- deparse(match.call())

  # Add attributes to the info_message
  attr_names <- names(attributes(variable))
  if (!is.null(attr_names)) {
    for (attr_name in attr_names) {
      attr_value <- attributes(variable)[[attr_name]]
      attr_value_str <- if (isTooLarge(attr_value)) paste("<BLOB of size", format(object.size(attr_value), units = "auto"), ">") else paste(attr_value, collapse = ", ")
      info_message <- paste0(info_message, "\n\t", attr_name, ": ", attr_value_str)
    }
  }

  # Handle reactive values
  if (is.reactivevalues(variable)) {
    reactive_contents <- reactiveValuesToList(variable)
    for (name in names(reactive_contents)) {
      value <- reactive_contents[[name]]
      value_type <- class(value)

      if (is.null(value)) {
        value_str <- "NULL"
      } else if (isTooLarge(value)) {
        value_str <- paste("<BLOB of size", format(object.size(value), units = "auto"), ">")
      } else {
        value_str <- paste(value, collapse = ", ")
      }

      info_message <- paste(info_message, "\n\t", name, ": ", value_str, " (Type: ", value_type, ")")
    }
  } else {
    # Handle non-reactive values
    if (isTooLarge(variable)) {
      value_str <- paste("<BLOB of size", format(object.size(variable), units = "auto"), ">")
    } else {
      value_str <- paste(variable, collapse = ", ")
    }

    info_message <- paste0(info_message, "\n\tValue: ", value_str, "\n\tType: ", variable_type, "\n\tCalling code: ", call_string)
  }

  message(info_message)
  flush.console()
}
