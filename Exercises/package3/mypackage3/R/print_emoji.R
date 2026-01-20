#' @export

print.esommerfeld <- function(x, ...) {
  cat(
    crayon::green(
      "Ella",
      emoji::emoji("purple heart"),
      "s programming in R!\n"
    )
  )
  NextMethod()
}
