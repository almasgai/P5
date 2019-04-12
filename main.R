library(GA)

numGenerations <- 100
populationSize <- 50
pm <- 0.001
pc <- 0.75
a <- 0.5
b <- 0.5

# Initializing functions f1 & f2. f3 is the minimum between f1 & f2
f1 <- function(x1, x2, x3, x4) { 2 * x2 * x4 + x3 * (x1 - 2 * x4) }
f2 <- function(x1, x2, x3, x4) { 60000 / x3 * ((x1 - 2 * x4) ** 3) + 2 * x2 * x4 * ((4 * x4 ** 2) + 3 * x1 * (x1 - 2 * x4)) }
f3 <- function(f1, f2) { a * f1 + b * f2 }

# The general optimization goal for the I-Beam problem is to minimize F=[f1,f2]T 
# where the cross section area (f1) and the static deflection (f2) of the I-beam.

GA1 <- ga(type="real-valued", fitness=f1, lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 4.0, 5.0),  pmutation = pm,  pcrossover = pc, popSize = populationSize)
GA2 <- ga(type="real-valued", fitness=f2, lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 4.0, 5.0), pmutation = pm,  pcrossover = pc, popSize = populationSize)

plot(f1(x1, x2, x3, x4),
     main = "Cross Section Area (f1)",
     xlab = "Generation No.", 
     ylab = "Best Particle Fitness")

plot(f2(x1, x2, x3, x4),
     main = "Static Delfection (f2)",
     xlab = "Generation No.", 
     ylab = "Best Particle Fitness")

summary(x1, x2, x3, x4)

plot(GA1)
summary(GA1)

plot(GA2)
summary(GA2)
