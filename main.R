f1 <- function () {
  X1 <- runif(1, 10, 80, replace=TRUE)
  X2 <- runif(1, 10, 50, replace=TRUE)
  X3 <- runif(1, 0.9, 5.0, replace=TRUE)
  X4 <- runif(1, 0.9, 5.0, replace=TRUE)
  result = 2 * X2 * X4 + X3 * (X1 - 2 * X4)
  return(result)
}
