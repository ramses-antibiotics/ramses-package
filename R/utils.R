.print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

.throw_error_DBI_subclass_not_implemented <- function(subclass) {
  stopifnot(is.character(subclass) & length(subclass) == 1)
  stop(paste(
    subclass,
    "is not implemented for this type of database.",
    "Please report this issue on https://github.com/ramses-antibiotics/ramses-package/issues",
    collapse = "\n"
  ))
}
