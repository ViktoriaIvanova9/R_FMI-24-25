# ======= 1 =========

sample_covariance <- function(x, y, na.rm=FALSE) {
  
  if(!is.numeric(x) || !is.numeric(y)) {
    return("All values of the vectors should be numerical.")
  }
  
  if(length(x) != length(y)) {
    return("Vectors should be with the same length.")
  }
  
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  n <- length(x)
  
  covariance <- sum((x - mean_x) * (y - mean_y)) / (n - 1)
  covariance
}

# ======= 2 =========

sample_std <- function(x, na.rm = FALSE) {
  n <- length(x)
  std <- sqrt( sum( ( x - mean(x))^2 ) / (n - 1))
  std
}

sample_correlation <- function(x, y, na.rm=FALSE) {
  covariance <- sample_covariance(x, y, na.rm)
  
  std_x <- sample_std(x, na.rm = TRUE)
  std_y <- sd(y)
  
  correlation <- covariance / (std_x * std_y)
  correlation
}

x <- c(1, 2, 3, 5, 6, 7, 8)
y <- c(1, 2, 3, 4, 6, 8, 9)

sample_covariance(x, y, na.rm=TRUE)
sample_correlation(x, y , na.rm=TRUE)

# ======= 3 =========

library(ggplot2)

ggplot(data = diamonds)
?diamonds
summary(diamonds)

# ======= 4 =========

library(dplyr)

constraint_diamonds <- diamonds %>%
  filter(price >= 600)
 
constraint_diamonds

# ======= 5 =========

price_per_cut <- diamonds %>%         
  group_by(cut) %>%
  summarise(avg_price = mean(price)) %>%
  arrange(avg_price)
  
price_per_cut

highest_cut <- price_per_cut %>% slice(n())
print("Highest average cut price: ")
highest_cut

lowest_cut <- price_per_cut %>% slice(1)
print("Lowest average cut price: ")
lowest_cut

# ======= 6 =========

constraint_diamonds <- constraint_diamonds %>%
  mutate(log_carat = log(carat), log_price = log(price))

log_carat <- constraint_diamonds$log_carat
log_price <- constraint_diamonds$log_price

correlation <- sample_correlation(log_carat, log_price)
correlation

ggplot(data = constraint_diamonds) +
  geom_point(mapping = aes(x = log_carat, y = log_price))

# Correlation - 0.96, close to 1 - strong linear relationship
# Can be seen also from the plot

# ======= 7 =========

I1_IF_clarity_diamonds <- constraint_diamonds %>%
  filter(clarity == "I1" | clarity == "IF")

ggplot(data = I1_IF_clarity_diamonds) +
  geom_point(mapping = aes(x = log_carat, y = log_price, color=clarity)) + 
  geom_smooth(mapping = aes(x = log_carat, y = log_price, color=clarity))

# Would you say that the diamonds from the two groups differ? - yes, they are separate 
# from each other, not very much overlaping
