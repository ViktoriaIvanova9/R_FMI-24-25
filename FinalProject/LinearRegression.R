library(tidymodels)
library(corrplot)

data(mtcars)

mtcars
dim(mtcars)
summary(mtcars)

# Which are the independent and dependent variables? - we can analyse the correlation heatmap
cor_matrix <- cor(mtcars)
corrplot(cor_matrix, method = "color")

wt_feature = mtcars$wt
mpg_feature = mtcars$mpg

plot(unlist(wt_feature), unlist(mpg_feature), 
     main="Observation of Cars", 
     xlab="Weight", 
     ylab="Milles/(US) gallon")

help(initial_splits)

set.seed(123)
data_split <- initial_split(mtcars, prop = 30/32)
train_data <- training(data_split)
test_data <- testing(data_split)

linreg <- linear_reg()
linreg_fit <- linreg %>%
  fit(mpg ~ wt, data = train_data)
linreg_fit

tidy(linreg_fit)
# What family of functions would the fitted model be a member of? -> polynomial - first degree
#                                                                    y = ax + b

# Write down the explicit form of f^(wt) = -5.36 * wt + 37.3

ggplot(train_data, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Fitted Linear Model",
       x = "Weight",
       y = "Miles/(US) gallon")


predictions <- predict(linreg_fit, new_data = test_data)
predictions

rmse_value <- augment(linreg_fit, new_data = test_data) %>%
          rmse(mpg, .pred)
rmse_value

