log_something <- function(x){
  tryCatch(
    error = function(cnd){
      NA
    },
    log(x)
  )
}

logish <- function(x){
  try(
    log(x)
  )
  print(x)
}

compounded <- function(P, r, n, t){
  if (P <= 0){
    message("Initial amount P is less than or equal to 0.")
  }
  P * (1 + r/n)^(n * t)
}

compounded(-1, 0.04, 12, 2)

x1 <- c(2, 3, 1, 4, 3, 10)
x2 <- c("a", "c", "e", "d", "b", "f")
x3 <- c(T, F, F, T, F, F)
x4 <- NA

sortAndConnect <- function(x){
  sorted <- sort(x)
  connected <- paste(sorted, collapse = "-")
  return(connected)
}
range <- function(x){
  rangeVal <- max(x) - min(x)
  return(rangeVal)
}

a <- 10
makeAdder <- function(x){
  function(y) x + y
}
adder5 <- makeAdder(5)