numGenerations = 100
populationSize = 50
pm = 0.001
pc = 0.75
a = 0.5
b = 0.5


f1 <- function () {
  X1 <- runif(1, 10, 80, replace=TRUE)
  X2 <- runif(1, 10, 50, replace=TRUE)
  X3 <- runif(1, 0.9, 5.0, replace=TRUE)
  X4 <- runif(1, 0.9, 5.0, replace=TRUE)
  result <- 2 * X2 * X4 + X3 * (X1 - 2 * X4)
  return(result)
}

f2 <- function () {
  X1 <- runif(1, 10, 80, replace=TRUE)
  X2 <- runif(1, 10, 50, replace=TRUE)
  X3 <- runif(1, 0.9, 5.0, replace=TRUE)
  X4 <- runif(1, 0.9, 5.0, replace=TRUE)
  result <- 60000 / X3 * ((X1 - 2 * X4) ** 3) + 2 * X2 *X4 * ((4 * X4) ** 2 + 3 * X1 * (X1 - 2 * X4))
  return(result)
}


