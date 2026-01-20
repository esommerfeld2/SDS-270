#' Fibonacci Function
#'
#' @description Return the first n values in the fibonacci sequence.
#'
#' @details n can not be negative or 0. The function takes n and creates a loop that for each value from 1 to n the fibonacci sequence is calculated and the next number is printed. The loop ends when n is reached.
#' @param n a number that represent the number of values you want returned
#'
#' @returns The output is a printed sequence of numbers. This is the fibonacci sequence up to n.
#' @export
#'
#' @examples fibonacci(2)
#' fibonacci(4)
fibonacci <- function(n){
  a <- 0
  b <- 1
  for (i in 1:n) {
    cat(a, " ")
    next_num <- a + b
    a <- b
    b <- next_num
  }
}

#Code used from https://www.geeksforgeeks.org/r-language/r-program-to-print-the-fibonacci-sequence/ as I did not know what the fibanci sequence was
