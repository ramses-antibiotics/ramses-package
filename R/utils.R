.print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

.throw_error_method_not_implemented <- function(function_name, class_name = "") {
  stopifnot(is.character(function_name) & length(function_name) == 1)
  stopifnot(is.character(class_name) & length(class_name) == 1)
  stop(paste(
    function_name,
    "is not implemented for",
    class_name,
    "objects.",
    "Please report this issue on https://github.com/ramses-antibiotics/ramses-package/issues",
    collapse = "\n"
  ))
}
