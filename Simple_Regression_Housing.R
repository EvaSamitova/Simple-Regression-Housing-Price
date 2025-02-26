# Load required libraries
library(tidyverse)
library(tidymodels)

# Load the dataset from GitHub
housing_data <- read_csv("https://raw.githubusercontent.com/EvaSamitova/Simple-Regression-Housing-Price/main/melbourne_housing.csv")

# Examine the data structure
str(housing_data)
problems(housing_data)

# 4 Check for missing values
sum(is.na(housing_data))

# 5 Drop rows with missing values
housing_data <- na.omit(housing_data)


# 6 Check correlation between Price and numeric variables
cor_matrix <- cor(select(housing_data, where(is.numeric)), use = "complete.obs")
print(cor_matrix["Price", ])

# 7 Scatter plot for Price vs BuildingArea
ggplot(housing_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Price vs Building Area", x = "Building Area", y = "Price")

# 8 Split data into training (75%) and testing (25%)
set.seed(123)  # For reproducibility
data_split <- initial_split(housing_data, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# 9 Define function for detecting outliers
calculate_fences <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR
  return(c(lower_fence, upper_fence))
}

# 10 Apply function to detect outliers in Price and BuildingArea
price_fences <- calculate_fences(train_data$Price)
building_fences <- calculate_fences(train_data$BuildingArea)

# 11 Remove outliers from training data
train_data <- train_data %>%
  filter(Price >= price_fences[1] & Price <= price_fences[2],
         BuildingArea >= building_fences[1] & BuildingArea <= building_fences[2])

# 12 Scatter plot after removing outliers
ggplot(train_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot After Removing Outliers", x = "Building Area", y = "Price")

# 13 Create linear regression model
model <- lm(Price ~ BuildingArea, data = train_data)
summary(model)  # Display model summary

# 14 Predict price on test data
test_data$PredictedPrice <- predict(model, newdata = test_data)

# 15 Scatter plot of actual vs predicted values
ggplot(test_data, aes(x = Price, y = PredictedPrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Actual vs Predicted Prices", x = "Actual Price", y = "Predicted Price")

# 16 Display model equation
intercept <- coef(model)[1]
coefficient <- coef(model)[2]
cat("Model Equation: Price =", intercept, "+", coefficient, "* BuildingArea\n")

# 17 Plot equation line on test data
ggplot(test_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_function(fun = function(x) intercept + coefficient * x, color = "blue") +
  labs(title = "Regression Line on Test Data", x = "Building Area", y = "Price")

# 18 Smooth trend line using geom_smooth
ggplot(test_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Smoothed Regression Line", x = "Building Area", y = "Price")

#Update code to show plots
ggplot(housing_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Price vs Building Area", x = "Building Area", y = "Price")
ggsave("scatter_price_building.png", width = 6, height = 4, dpi = 300)

#Update code to show plots after
ggplot(train_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot After Removing Outliers", x = "Building Area", y = "Price")
ggsave("scatter_no_outliers.png", width = 6, height = 4, dpi = 300)

# Create the scatter plot
p <- ggplot(housing_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Price vs Building Area", x = "Building Area", y = "Price")

# Print the plot to verify
print(p)

# Save the plot explicitly
ggsave(filename = "scatter_price_building.png", plot = p, width = 6, height = 4, dpi = 300)


#Save Actual vs Predicted Prices Plot
ggplot(test_data, aes(x = Price, y = PredictedPrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Actual vs Predicted Prices", x = "Actual Price", y = "Predicted Price")
ggsave("actual_vs_predicted.png", width = 6, height = 4, dpi = 300)

#Save Regression Line on Test Data
ggplot(test_data, aes(x = BuildingArea, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_function(fun = function(x) intercept + coefficient * x, color = "blue") +
  labs(title = "Regression Line on Test Data", x = "Building Area", y = "Price")
ggsave("regression_line.png", width = 6, height = 4, dpi = 300)
