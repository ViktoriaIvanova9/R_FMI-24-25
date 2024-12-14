library(purrr)

sample_raw_moment_factory <- function(k) {
  raw_moment <- function(num_vector) {
    mean(num_vector^k, na.rm=TRUE)
  }
  raw_moment
}

raw_moment_2 <- function(num_vector) {
  k <- 2
  raw_moment_func <- sample_raw_moment_factory(k)
  raw_moment_func(num_vector)
}

numerical_vector <- c(1, NA, 2, 3, NA, 4, 5, 6)
raw_moment_2(numerical_vector)

calculate_numeric_column_stats <- function(func_to_apply) {
  function(df) {
    purrr::map_if(df, is.numeric, func_to_apply)
  }
}

calculate_raw_moment_2 <- function(df) {
  list_func <- calculate_numeric_column_stats(raw_moment_2)
  list_func(df)
}

cars_df <- mtcars
result <- calculate_raw_moment_2(cars_df)

result
typeof(result)

