library(GA)

numGenerations <- 100
populationSize <- 50
pm <- 0.001
pc <- 0.75
a <- 0.5
b <- 0.5

x1 <- runif(1, 10, 80)
x2 <- runif(1, 10, 50)
x3 <- runif(1, 0.9, 5.0)
x4 <- runif(1, 0.9, 5.0)

# Initializing functions f1 & f2. f3 is the minimum between f1 & f2
f1 <- function(x1 = runif(1, 10, 80), x2 = runif(1, 10, 50), x3 = runif(1, 0.9, 5.0), x4 = runif(1, 0.9, 5.0)) { 
  2 * x2 * x4 + x3 * (x1 - 2 * x4) 
}

f2 <- function(x1 = runif(1, 10, 80), x2 = runif(1, 10, 50), x3 = runif(1, 0.9, 5.0), x4 = runif(1, 0.9, 5.0)) { 
  60000 / (x3 * ((x1 - 2 * x4) ** 3) + 2 * x2 * x4 * ((4 * x4 ** 2) + 3 * x1 * (x1 - 2 * x4))) 
}

f3 <- function() { 
  x1 <- runif(1, 10, 80)
  x2 <- runif(1, 10, 50)
  x3 <- runif(1, 0.9, 5.0)
  x4 <- runif(1, 0.9, 5.0)
  a * f1(x1, x2, x3, x4) + b * f2(x1, x2, x3, x4)
}

# The general optimization goal for the I-Beam problem is to minimize F=[f1,f2]T 
# where the cross section area (f1) and the static deflection (f2) of the I-beam.

GA1 <- ga(type="real-valued", fitness=f1, lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 5.0, 5.0), pmutation = pm, pcrossover = pc, popSize = populationSize)
GA2 <- ga(type="real-valued", fitness=f2, lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 5.0, 5.0), pmutation = pm, pcrossover = pc, popSize = populationSize)
GA3 <- ga(type="real-valued", fitness=f3, lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 5.0, 5.0), pmutation = pm, pcrossover = pc, popSize = populationSize)

plot(f1, main = "Cross Section Area (f1)", xlab = "Generation No.", ylab = "Best Particle Fitness")
plot(f2, main = "Static Deflection (f2)", xlab = "Generation No.", ylab = "Best Particle Fitness")
plot(f3, main = "F = a * f1 + b * f2", xlab = "Generation No.", ylab = "Best Particle Fitness")

plot(GA1)
plot(GA2)