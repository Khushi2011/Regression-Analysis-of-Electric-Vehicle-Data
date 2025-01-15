#Name: Group 5
#Date:28th April 2024
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
options(scipen = 100)

library(pacman)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(psych)
library(readxl)
library(tidyr)
library(regclass)
library(janitor)
library(ggplot2)
p_load(geosphere)
p_load(ggmap)
p_load(VIM)
library(glmnet)
library(viridis)

# Load the Electrical Vehicle dataset.
electric_vehicle_data = read.csv("/Users/wanshicheng/Desktop/NorthEastern/Term2/Aly 6015/Final Project/Electric_Vehicle_Population_Data.csv")
View(electric_vehicle_data)

# Clean the column names.
electric_vehicle_data <- clean_names(electric_vehicle_data)
names(electric_vehicle_data)


# Get column names with missing values
cols_with_na <- colnames(electric_vehicle_data)[colSums(is.na(electric_vehicle_data)) > 0]
cols_with_na

#---------------------------------------------legislative_district-----------------------------------------------
# Calculate the median of the 'Legislative District' column, assuming it's numeric
median_value <- median(electric_vehicle_data$legislative_district, na.rm = TRUE)


# Replace NA values with the median
electric_vehicle_data$legislative_district <- ifelse(is.na(electric_vehicle_data$legislative_district), median_value, electric_vehicle_data$legislative_district)


# Optionally, you can use the mutate function from dplyr to perform the imputation
electric_vehicle_data <- electric_vehicle_data %>%
  mutate(legislative_district = ifelse(is.na(legislative_district), median_value, legislative_district))


cols_with_na <- colnames(electric_vehicle_data)[colSums(is.na(electric_vehicle_data)) > 0]
cols_with_na

#----------------------------------------------x2020_census_tract----------------------------------------------
library(tidyverse)  # for data manipulation
library(caret)  
# Set a seed for reproducibility
set.seed(123)

# Impute missing values using KNN
electric_vehicle_data <- kNN(electric_vehicle_data, variable = "x2020_census_tract", k = 5)
cols_with_na <- colnames(electric_vehicle_data)[colSums(is.na(electric_vehicle_data)) > 0]
cols_with_na

#----------------------------------------------Postal Code----------------------------------------------
library(readr)
library(stringr)

# Set up Google Maps API key
register_google(key = "AIzaSyCdjlhCcwR-3X80oOq3aX34F8aQck0qSos") 

electric_vehicle_data <- electric_vehicle_data %>%
  rowwise() %>%
  mutate(
    clean_location = gsub("POINT \\((.*?)\\)", "\\1", vehicle_location),
    parts = strsplit(clean_location, " ")
  ) %>%
  ungroup()

# Separate the clean_location column into longitude and latitude
electric_vehicle_data <- electric_vehicle_data %>%
  separate(clean_location, into = c("longitude", "latitude"), sep = " ")


# Convert the longitude and latitude columns to numeric
electric_vehicle_data$longitude <- as.numeric(electric_vehicle_data$longitude)
electric_vehicle_data$latitude <- as.numeric(electric_vehicle_data$latitude)


#head(electric_vehicle_data$longitude)
na_indices <- which(is.na(electric_vehicle_data$postal_code))
na_indices

# Function to fetch postal code using ggmap
get_postal_code <- function(lat, long) {
  if (!is.na(lat) && !is.na(long)) {
    location <- revgeocode(c(long, lat), output = "address")
    postal_code <- sub(".*\\b([0-9]{5}(?:-[0-9]{4})?)$", "\\1", location)
    return(postal_code)
  } else {
    return(NA)
  }
}

for (i in na_indices) {
  postal_code <- get_postal_code(electric_vehicle_data$latitude[i], electric_vehicle_data$longitude[i])
  # Extract the postal code
  postal_code <- sub(".*\\b(\\d{5})[^\\d]*$", "\\1", postal_code)
  electric_vehicle_data$postal_code[i] <- postal_code
}


na_indices <- which(is.na(electric_vehicle_data$postal_code))
na_indices

# Removing the rows which doesn't have any value of latitude and longitude
electric_vehicle_data <- electric_vehicle_data[setdiff(seq_len(nrow(electric_vehicle_data)), na_indices), ]
electric_vehicle_data <- electric_vehicle_data[, !(names(electric_vehicle_data) %in% c("longitude", "latitude", "parts", "x2020_census_tract_imp"))]

cols_with_na <- colnames(electric_vehicle_data)[colSums(is.na(electric_vehicle_data)) > 0]
cols_with_na

#------------------------------------------------------------------------------------------------
# Here checking if there are still null values or not
sapply(electric_vehicle_data, function(x) sum(is.na(x))) 

# Perform Exploratory Data Analysis and use descriptive statistics to describe the data.
head(electric_vehicle_data)

names(electric_vehicle_data)

str(electric_vehicle_data)
summary(electric_vehicle_data)
describe(electric_vehicle_data)

# ===============Outliers================


# A correlation matrix of the numeric values.
# Calculate the correlation matrix
correlation_matrix <- cor(electric_vehicle_data[, sapply(electric_vehicle_data, is.numeric)])
correlation_matrix


library(corrplot)
p_load(RColorBrewer)
corrplot(correlation_matrix, col = brewer.pal( n = 8, name = "RdYlBu"))


theme_set(theme_bw())
ggplot(electric_vehicle_data, aes(x = model_year)) +
  geom_bar() +
  labs(title = 'Distribution of Model Year') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of Electric Vehicle Type
ggplot(electric_vehicle_data, aes(x = electric_vehicle_type)) +
  geom_bar() +
  labs(title = 'Distribution of Electric Vehicle Type') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribution of Top 10 Makes
electric_vehicle_data %>%
  count(make) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(make, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = 'Distribution of Top 10 Makes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribution of Electric Vehicle Type across Model Years
ggplot(electric_vehicle_data, aes(x = model_year, fill = electric_vehicle_type)) +
  geom_bar(position = "dodge") +
  labs(title = 'Distribution of Electric Vehicle Types Across Model Years') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Electric Vehicle Type"))



# Identify numerical and categorical variables in the dataset
classification <- sapply(electric_vehicle_data, class)

# Separate numerical and categorical variables based on their class
categorical_vars <- names(classification[classification %in% c('factor', 'character')])
numerical_vars <- names(classification[!(classification %in% c('factor', 'character'))])

# Print the lists of numerical and categorical variables
print(list('Categorical Variables' = categorical_vars, 'Numerical Variables' = numerical_vars))


# -----------------------Q1Primary Factors Influencing EV Adoption------------------------------#
# Prepare data for visualization: Distribution of EVs by luxury vs. standard brands
ev_brands <- electric_vehicle_data %>% 
  mutate(Brand_Type = ifelse(make %in% c('TESLA', 'BMW', 'AUDI', 'MERCEDES'), 'Luxury', 'Standard')) %>% 
  group_by(Brand_Type) %>% 
  summarise(EV_Count = n(), .groups = 'drop')

# Create a pie chart for the distribution of luxury vs standard EV brands
ggplot(ev_brands, aes(x = '', y = EV_Count, fill = Brand_Type)) + 
  geom_bar(width = 1, stat = 'identity') + 
  coord_polar(theta = 'y') + 
  labs(title = 'Distribution of EVs by Brand Type', fill = 'Brand Type') + 
  theme_minimal()

## Analysis
# Prepare data for correlation analysis: Number of electric utilities and EV adoption
ev_infrastructure <- electric_vehicle_data %>% 
  group_by(county) %>% 
  summarise(EV_Count = n(), Utility_Count = n_distinct(electric_utility), .groups = 'drop')

# Calculate the correlation between EV_Count and Utility_Count
correlation_result <- cor(ev_infrastructure$EV_Count, ev_infrastructure$Utility_Count)

# Output the correlation result
correlation_result

#------------------ vehicle specifications on consumer decisions-----------------------#

# Prepare data for scatter plot visualization
# Using 'electric_range', 'model_year', and 'EV_Count' as variables
ev_specs_analysis <- electric_vehicle_data %>% 
  group_by(model_year, electric_range) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  filter(electric_range > 0) # Filtering out entries with zero or undefined electric range

# Create scatter plots for electric range vs model year with size indicating the number of EV registrations
ggplot(ev_specs_analysis, aes(x = model_year, y = electric_range, size = EV_Count)) + 
  geom_point(alpha = 0.6) + 
  labs(title = 'Relationship Between Electric Range, Model Year, and EV Registrations', x = 'Model Year', y = 'Electric Range') + 
  theme_minimal()

## analysis 
# Prepare data for regression analysis
# Using 'electric_range', 'model_year', and 'EV_Count' as variables for the model
ev_regression_data <- electric_vehicle_data %>% 
  group_by(model_year, electric_range) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  filter(electric_range > 0) # Filtering out entries with zero or undefined electric range

# Fit a linear regression model to quantify the impact
model <- lm(EV_Count ~ model_year + electric_range, data = ev_regression_data)

# Output the summary of the regression model
summary(model)


#-------------Role of Geographic Location in EV Adoption:---------------------------#
# Prepare data for heatmap visualization
# Group data by county and calculate EV adoption rates
ev_geo_data <- electric_vehicle_data %>% 
  group_by(county) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  mutate(Adoption_Rate = EV_Count / sum(EV_Count) * 100)

## create the fictional geographic data for counties
counties <- data.frame(county = unique(ev_geo_data$county), lat = runif(n = length(unique(ev_geo_data$county)), min = 34, max = 42), long = runif(n = length(unique(ev_geo_data$county)), min = -124, max = -114))
# Ensure proper merging of the data
ev_geo_data <- merge(ev_geo_data, counties, by = 'county', all.x = TRUE)
# Attempt to plot again using geom_point for visibility
ggplot(ev_geo_data, aes(x = long, y = lat, color = Adoption_Rate)) + 
  geom_point(size = 5, alpha = 0.7) + 
  scale_color_viridis(name = "Adoption Rate (%)", option = "C") + 
  labs(title = 'Corrected Map of EV Adoption Rates Across Different Regions', x = 'Longitude', y = 'Latitude') + 
  theme_minimal()

# Get a map of the contiguous US
us_map <- get_map(location = c(lon = -98, lat = 39),zoom=4 ,maptype = 'roadmap')

# Create a ggmap with the EV adoption rates data
ggmap(us_map) + 
  geom_point(data = ev_geo_data, aes(x = long, y = lat, color = Adoption_Rate, size = Adoption_Rate), alpha = 0.7) + 
  scale_color_viridis(name = "Adoption Rate (%)", option = "C") + 
  labs(title = 'EV Adoption Rates Across Different US Regions', x = 'Longitude', y = 'Latitude') + 
  theme_minimal()

## Analysis
#--------------------- Analyze regional policies and infrastructure affecting EV adoption-----------
# Group data by legislative district and electric utility to infer policy and infrastructure influences
ev_regional_analysis <- electric_vehicle_data %>% 
  group_by(legislative_district, electric_utility) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  mutate(Infrastructure_Score = EV_Count / sum(EV_Count) * 100)

# Summarize the findings
summary(ev_regional_analysis)

# Policies to Increase EV Adoption:
# Prepare data for comparative analysis visualization
# Identify regions with high and low EV adoption rates
ev_high_low <- electric_vehicle_data %>% 
  group_by(county) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  mutate(Adoption_Rate = EV_Count / sum(EV_Count) * 100) %>% 
  arrange(desc(Adoption_Rate))


# Select top 5 and bottom 5 counties based on adoption rate for visualization
top_5 <- head(ev_high_low, 5)
bottom_5 <- tail(ev_high_low, 5)

# Combine for comparative visualization
ev_compare <- rbind(top_5, bottom_5)

# Create a bar plot to compare high and low adoption regions
ggplot(ev_compare, aes(x = reorder(county, Adoption_Rate), y = Adoption_Rate, fill = factor(county))) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette = 'Set3') + 
  labs(title = 'Comparative Analysis of High and Low EV Adoption Regions', x = 'County', y = 'Adoption Rate (%)') + 
  theme_minimal()
## analysis
# Analyze policies in high-adoption areas
# Extract data for high-adoption counties and their associated policies
ev_policies_high <- electric_vehicle_data %>% 
  filter(county %in% top_5$county) %>% 
  group_by(county, legislative_district, electric_utility) %>% 
  summarise(EV_Count = n(), .groups = 'drop') %>% 
  mutate(Infrastructure_Score = EV_Count / sum(EV_Count) * 100)

# Summarize the policies and infrastructure scores for high-adoption areas
summary(ev_policies_high)


# --------------------Predicting Future Trends in EV Adoption:-----------------------------------#
# Prepare data for line graph of historical and projected EV adoption trends using 'model_year'
ev_trend_data <- electric_vehicle_data %>% 
  group_by(model_year) %>% 
  summarise(EV_Count = n(), .groups = 'drop')

# Generate a simple linear projection based on historical data
model <- lm(EV_Count ~ model_year, data = ev_trend_data)
future_years <- data.frame(model_year = seq(max(ev_trend_data$model_year) + 1, max(ev_trend_data$model_year) + 10))
projections <- predict(model, newdata = future_years)

# Combine historical and projected data for visualization
ev_trend_data <- rbind(ev_trend_data, data.frame(model_year = future_years$model_year, EV_Count = projections))

# Create a line graph
ggplot(ev_trend_data, aes(x = model_year, y = EV_Count)) + 
  geom_line() + 
  geom_point() + 
  labs(title = 'Historical and Projected EV Adoption Trends', x = 'Model Year', y = 'EV Count') + 
  theme_minimal()
## analysis 
# Develop a regression model to forecast future EV adoption trends
# Using the historical data prepared earlier
model <- lm(EV_Count ~ model_year, data = ev_trend_data)

# Summary of the regression model to understand the coefficients and accuracy
summary(model)


# ===============Outliers================
# Now, calculate IQR and identify outliers for 'electric_range' and 'base_msrp'
# Selecting the relevant columns
data <- electric_vehicle_data[, c('electric_range', 'base_msrp')]

# Calculating the IQR for each column
electric_range_iqr <- IQR(data$electric_range)
base_msrp_iqr <- IQR(data$base_msrp)

# Calculating the bounds for outliers
electric_range_bounds <- quantile(data$electric_range, c(0.25, 0.75)) + c(-1.5, 1.5) * electric_range_iqr
base_msrp_bounds <- quantile(data$base_msrp, c(0.25, 0.75)) + c(-1.5, 1.5) * base_msrp_iqr

# Identifying outliers
electric_range_outliers <- data[data$electric_range < electric_range_bounds[1] | data$electric_range > electric_range_bounds[2], ]
base_msrp_outliers <- data[data$base_msrp < base_msrp_bounds[1] | data$base_msrp > base_msrp_bounds[2], ]

# Output the number of outliers and the bounds for each column
list(electric_range_outliers = nrow(electric_range_outliers), base_msrp_outliers = nrow(base_msrp_outliers), electric_range_bounds = electric_range_bounds, base_msrp_bounds = base_msrp_bounds)




# Investigate the 'base_msrp' data for anomalies or errors
base_msrp_summary <- summary(electric_vehicle_data$base_msrp)
base_msrp_unique_values <- unique(electric_vehicle_data$base_msrp)

# Output the summary and unique values of 'base_msrp'
list(base_msrp_summary = base_msrp_summary, base_msrp_unique_values = base_msrp_unique_values)


# Adjusting the imputation logic for 'base_msrp'
# Ensure the dataset is loaded
electric_vehicle_data <- read.csv('Updated_Electric_Vehicle.csv', stringsAsFactors = FALSE)

# Calculate the median 'base_msrp' for each group, excluding $0 values
grouped_medians <- aggregate(base_msrp ~ model_year + electric_vehicle_type, data = electric_vehicle_data[electric_vehicle_data$base_msrp > 0, ], FUN = median)

# Merge the median values back to the original data
electric_vehicle_data <- merge(electric_vehicle_data, grouped_medians, by = c('model_year', 'electric_vehicle_type'), suffixes = c('', '_median'))

# Replace $0 with the median value from the same group
electric_vehicle_data$base_msrp[electric_vehicle_data$base_msrp == 0] <- electric_vehicle_data$base_msrp_median[electric_vehicle_data$base_msrp == 0]

# Remove the temporary median column
electric_vehicle_data$base_msrp_median <- NULL

# Summary of 'base_msrp' after imputation
base_msrp_summary_post <- summary(electric_vehicle_data$base_msrp)

# Output the summary of 'base_msrp' after imputation
list(base_msrp_summary_post = base_msrp_summary_post)











# Apply log transformation to 'electric_range' to reduce the impact of extreme values
# Add a small constant to avoid log(0) issue
electric_vehicle_data$electric_range_log <- log(electric_vehicle_data$electric_range + 1)

# Summary of 'electric_range' after log transformation
log_transformed_summary <- summary(electric_vehicle_data$electric_range_log)

# Output the summary of 'electric_range' after log transformation
list(log_transformed_summary = log_transformed_summary)





# Explore the relationship between 'base_msrp' and 'electric_range_log' using a scatter plot
library(ggplot2)

# Create a scatter plot to visualize the relationship
scatter_plot <- ggplot(electric_vehicle_data, aes(x = electric_range_log, y = base_msrp)) +
  geom_point(alpha = 0.5) +
  labs(title = 'Relationship between Base MSRP and Electric Range (Log Transformed)', x = 'Log Transformed Electric Range', y = 'Base MSRP') +
  theme_minimal()

# Display the plot
print(scatter_plot)










#Create a bar plot for the manufacture
barplot(table(electric_vehicle_data$make), main = "Vehicles by Manufacturer", xlab = "Manufacturer", ylab = "Number of Vehicles", col = "red", las = 2)

#Generate a scatter plot to explore relationships between Model year and Range
plot(electric_vehicle_data$model_year, electric_vehicle_data$electric_range, main = "Model Year vs. Range", xlab = "Model Year", ylab = "Range (miles)", pch = 19, col = "green")

ggplot(electric_vehicle_data, aes(x = base_msrp, y = electric_range)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Scatter Plot with Regression Line", x = "Base msrp($)", y = "Range (miles)")

#create the boxplot between electric vehicle type and range
ggplot(electric_vehicle_data, aes(x = electric_vehicle_type, y = electric_range)) +
  geom_boxplot() +
  labs(title = "Range by Electric Vehicle Type",
       x = "Vehicle Type",
       y = "Range (miles)") +
  theme_minimal()

#create the boxplot between make and base msrp
ggplot(electric_vehicle_data, aes(x = make, y = base_msrp)) +
  geom_boxplot() +
  labs(title = "Base MSRP by Vehicle Make",
       x = "Make",
       y = "Base MSRP (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



#------------------------------------Chi square and Anova--------------------------------
#add your code here shicheng
# First, inspect the range and distribution of electric_range
summary(electric_vehicle_data$electric_range)
hist(electric_vehicle_data$electric_range, main = "Distribution of Electric Range", xlab = "Electric Range")

# Define breaks based on observed distribution
breaks <- c(0, 50, 100, 150, 200, 250, max(electric_vehicle_data$electric_range, na.rm = TRUE))
electric_vehicle_data$range_category <- cut(electric_vehicle_data$electric_range, breaks = breaks, include.lowest = TRUE, labels = FALSE)
electric_vehicle_data$electric_range <- ifelse(is.na(electric_vehicle_data$electric_range), median(electric_vehicle_data$electric_range, na.rm = TRUE), electric_vehicle_data$electric_range)

# Ensure vehicle_type is a factor
electric_vehicle_data$electric_vehicle_type <- as.factor(electric_vehicle_data$electric_vehicle_type)

# Creating a contingency table
contingency_table <- table(electric_vehicle_data$electric_vehicle_type, electric_vehicle_data$electric_range)
chi_square_result <- chisq.test(contingency_table)
chi_square_result

critical_value <- qchisq(p = 0.95, df = (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1))
test_statistic <- chi_square_result$statistic

print(paste("Critical Value:", critical_value))
print(paste("Test Statistic:", test_statistic))
if (test_statistic > critical_value) {
  cat("Reject the null hypothesis: There is significant evidence that vehicle type influences electric range.")
} else {
  cat("Fail to reject the null hypothesis: There is not sufficient evidence to conclude that vehicle type influences electric range.")
}

#Anova
electric_vehicle_data$legislative_district <- as.factor(electric_vehicle_data$legislative_district)
electric_vehicle_data$electric_range <- as.numeric(electric_vehicle_data$electric_range)
anova_model <- aov(electric_range ~ base_msrp * legislative_district, data = electric_vehicle_data)
summary(anova_model)


#------------------------------------Q2 regression model------------------------------------------

# Fit polynomial regression model
model <- lm(electric_range ~ poly(model_year, 2), data=electric_vehicle_data)

# Summary of the model
summary(model)

# Plot the data and regression line
plot(electric_vehicle_data$model_year, electric_vehicle_data$electric_range, xlab="Model Year", ylab="Electric Range")
lines(sort(electric_vehicle_data$model_year), predict(model, data.frame(model_year=sort(electric_vehicle_data$model_year))), col="red")


#-------------------------------Ridge and Lasso---------------------------------------

set.seed(123)
#Split the train and test dataset from the Original College dataset
# Sample 10,000 rows from train_data
sampled_rows <- sample(nrow(electric_vehicle_data), 5000)
data_sample <- electric_vehicle_data[sampled_rows, ]

# Create model matrix
#train_data_X <- model.matrix(electric_range ~ ., data = train_data_sample)[, -1]
trainIndex <- createDataPartition(data_sample$electric_range, p = 0.7, list = FALSE, times = 1)
train_data <- data_sample[ trainIndex,]
test_data <- data_sample[-trainIndex,]

train_data_X <- model.matrix(electric_range ~., train_data)[,-1]
test_data_X <- model.matrix(electric_range ~., test_data)[,-1]

train_data_y <- train_data$electric_range
test_data_y <- test_data$electric_range


set.seed(123)

# Sample 5000 rows from electric_vehicle_data
sampled_rows <- sample(nrow(electric_vehicle_data), 5000)
data_sample <- electric_vehicle_data[sampled_rows, ]

# Split data into train and test sets
trainIndex <- createDataPartition(data_sample$electric_range, p = 0.7, list = FALSE, times = 1)
train_data <- data_sample[trainIndex, ]
test_data <- data_sample[-trainIndex, ]

# Create model matrices for train and test sets
train_data_X <- model.matrix(electric_range ~ ., data = train_data)[, -1]
test_data_X <- model.matrix(electric_range ~ ., data = test_data)[, -1]

# Extract target variable (electric_range) for train and test sets
train_data_y <- train_data$electric_range
test_data_y <- test_data$electric_range

# Find the best value for lambda using Cross-validation
set.seed(123)
cv.ridge <- cv.glmnet(train_data_X, train_data_y, alpha = 0, nfolds = 10)
cv.ridge

# Estimation of the lambda.min and lambda.1se values
cv.ridge$lambda.min
cv.ridge$lambda.1se
# optimal value of lambda will minimizes the prediction error
log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

# Plot the results from the cv.glmnet function
plot(cv.ridge)

#4) Fit a Ridge regression model against the training set
model_ridge_min <- glmnet(train_data_X, train_data_y, alpha = 0, lambda = cv.ridge$lambda.min)
model_ridge_min
coef(model_ridge_min)

model_ridge_1se <- glmnet(train_data_X, train_data_y, alpha = 0, lambda = cv.ridge$lambda.1se)
model_ridge_1se
coef(model_ridge_1se)

#5) Calculating the root mean square error (RMSE) for train data
# Train set predictions
preds_train <- predict(model_ridge_1se, newx = train_data_X)
train_rmse_ridge <- RMSE(train_data_y, preds_train) #sqrt(mean((train_data_y - preds_train)^2))
train_rmse_ridge

#6) Calculating the root mean square error (RMSE) for test data
# Test set predictions 
preds_test <- predict(model_ridge_1se, newx = test_data_X)
test_rmse_ridge <- RMSE(test_data_y, preds_test) 
test_rmse_ridge

# Check if the model is overfitting
if (train_rmse_ridge < test_rmse_ridge) {
  print("The model might be overfitting.")
} else {
  print("The model is not overfitting.")
}