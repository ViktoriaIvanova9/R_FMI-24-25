central_limit_theorem <- function(i, n, p) {
  
  if(i < 1000 || i > 10000 || !is.integer(i)) {
    stop("Number of iterations must be integer between 1000 and 10000.")
  }
  
  if(n < 90 || n > 900 || n%%2 != 0 || !is.integer(n)) {
    stop("Size of simulation must be even integer between 90 and 900.")
  }
  
  if(p < 0 || p > 1 || !is.numeric(p)) {
    stop("Probability should be real number between 0 and 1.")
  }
  
  mu <- (1 - p)/ p
  sigma <- sqrt((1 - p)/ (p^2))
  vector_size <- n / 2
  
  result_vector <- numeric(i)
  
  for(curr_iteration in 1:i) {
    normal_distribution_vector <- rnorm(vector_size, mu, sigma)
    geom_distribution_vector <- rgeom(vector_size, p)
    
    concatenated_vectors <- c(normal_distribution_vector, geom_distribution_vector)
    shuffled_vector <- sample(concatenated_vectors)

    mean_shuffled <- mean(shuffled_vector)
    result_to_add <- ((mean_shuffled - mu) / sigma) * sqrt(n)

    result_vector[curr_iteration] <- result_to_add
  }

  return(result_vector)
}

vector_central_limit_theorem <- central_limit_theorem(3000L, 150L, 0.35)

mean_res <- mean(vector_central_limit_theorem)
std_res <- sd(vector_central_limit_theorem)

sprintf("Mean: %.10f. Standard deviation: %.10f.", mean_res, std_res)


qqnorm(vector_central_limit_theorem, main = "Simulation vector results")
qqline(vector_central_limit_theorem, col = "red")

# Does it look like the values follow a normal distribution? - yes


