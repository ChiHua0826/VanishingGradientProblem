#install.packages('GA')
library("GA")

set.seed(0)

estimated_y <- function(x, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) {
  estimated_y <- x
  estimated_y <- activation(estimated_y, w1, b1)
  estimated_y <- activation(estimated_y, w2, b2)
  estimated_y <- activation(estimated_y, w3, b3)
  estimated_y <- activation(estimated_y, w4, b4)
  estimated_y <- activation(estimated_y, w5, b5)
  estimated_y <- activation(estimated_y, w6, b6)
  estimated_y <- activation(estimated_y, w7, b7)
  estimated_y <- activation(estimated_y, w8, b8)
  estimated_y <- activation(estimated_y, w9, b9)
}

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

activation <- function(x, w, b) {
  sigmoid(w * x + b)
}

mean_squred_error <- function(x, y, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) {
  (estimated_y(x, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) - y) * 
    (estimated_y(x, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) - y)
}

fitness_function <- function(w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9)
{
  mean_squred_error(0.1, 0.3, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) +
  mean_squred_error(0.2, 0.4, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) +
  mean_squred_error(0.3, 0.5, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) +
  mean_squred_error(0.4, 0.6, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9) +
  mean_squred_error(0.5, 0.7, w1, b1, w2, b2, w3, b3, w4, b4, w5, b5, w6, b6, w7, b7, w8, b8, w9, b9)
}
GA <- ga(type = "real-valued", 
         fitness = function(W) 
           - fitness_function(W[1], W[2], W[3], W[4], W[5], W[6], W[7], W[8], W[9], W[10], W[11], W[12], W[13], W[14], W[15], W[16], W[17], W[18]),
         lower = c(-20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20), 
         upper = c(20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20),
         pcrossover = 0.9, pmutation = 0.1,
         popSize = 1000, maxiter = 50000)


print(estimated_y(0.1, GA@solution[1,1], GA@solution[1,2], GA@solution[1,3], GA@solution[1,4], GA@solution[1,5], GA@solution[1,6], GA@solution[1,7], GA@solution[1,8], GA@solution[1,9], GA@solution[1,10], GA@solution[1,11], GA@solution[1,12], GA@solution[1,13], GA@solution[1,14], GA@solution[1,15], GA@solution[1,16], GA@solution[1,17], GA@solution[1,18]))
print(estimated_y(0.2, GA@solution[1,1], GA@solution[1,2], GA@solution[1,3], GA@solution[1,4], GA@solution[1,5], GA@solution[1,6], GA@solution[1,7], GA@solution[1,8], GA@solution[1,9], GA@solution[1,10], GA@solution[1,11], GA@solution[1,12], GA@solution[1,13], GA@solution[1,14], GA@solution[1,15], GA@solution[1,16], GA@solution[1,17], GA@solution[1,18]))
print(estimated_y(0.3, GA@solution[1,1], GA@solution[1,2], GA@solution[1,3], GA@solution[1,4], GA@solution[1,5], GA@solution[1,6], GA@solution[1,7], GA@solution[1,8], GA@solution[1,9], GA@solution[1,10], GA@solution[1,11], GA@solution[1,12], GA@solution[1,13], GA@solution[1,14], GA@solution[1,15], GA@solution[1,16], GA@solution[1,17], GA@solution[1,18]))
print(estimated_y(0.4, GA@solution[1,1], GA@solution[1,2], GA@solution[1,3], GA@solution[1,4], GA@solution[1,5], GA@solution[1,6], GA@solution[1,7], GA@solution[1,8], GA@solution[1,9], GA@solution[1,10], GA@solution[1,11], GA@solution[1,12], GA@solution[1,13], GA@solution[1,14], GA@solution[1,15], GA@solution[1,16], GA@solution[1,17], GA@solution[1,18]))
print(estimated_y(0.5, GA@solution[1,1], GA@solution[1,2], GA@solution[1,3], GA@solution[1,4], GA@solution[1,5], GA@solution[1,6], GA@solution[1,7], GA@solution[1,8], GA@solution[1,9], GA@solution[1,10], GA@solution[1,11], GA@solution[1,12], GA@solution[1,13], GA@solution[1,14], GA@solution[1,15], GA@solution[1,16], GA@solution[1,17], GA@solution[1,18]))

summary(GA)
plot(GA)
