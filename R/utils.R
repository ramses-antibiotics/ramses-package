.print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}