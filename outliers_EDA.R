###########
# Prepare data for visualization 2: Impact of vehicle specifications on consumer decisions
# Using 'electric_range' and 'model_year' as variables
ev_specs <- electric_vehicle_data %>%
  select(model_year, electric_range, county) %>%
  filter(electric_range > 0) # Filtering out entries with zero or undefined electric range
# Create scatter plots for electric range vs model year colored by county
ggplot(ev_specs, aes(x = model_year, y = electric_range, color = county)) +
  geom_point(alpha = 0.6) +
  labs(title = 'Impact of Electric Range and Model Year on EV Adoption', x = 'Model Year', y = 'Electric Range') +
  theme_minimal()
##1
# Prepare data for correlation analysis: Number of electric utilities and EV adoption
ev_infrastructure <- electric_vehicle_data %>%
  group_by(county) %>%
  summarise(EV_Count = n(), Utility_Count = n_distinct(electric_utility), .groups = 'drop')
##1
# Prepare data for correlation analysis: Number of electric utilities and EV adoption
ev_infrastructure <- electric_vehicle_data %>%
  group_by(county) %>%
  summarise(EV_Count = n(), Utility_Count = n_distinct(electric_utility), .groups = 'drop')
# Calculate the correlation between EV_Count and Utility_Count
correlation_result <- cor(ev_infrastructure$EV_Count, ev_infrastructure$Utility_Count)
# Output the correlation result
correlation_result
## 2
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
###
# Analyze regional policies and infrastructure affecting EV adoption
# Group data by legislative district and electric utility to infer policy and infrastructure influences
ev_regional_analysis <- electric_vehicle_data %>%
  group_by(legislative_district, electric_utility) %>%
  summarise(EV_Count = n(), .groups = 'drop') %>%
  mutate(Infrastructure_Score = EV_Count / sum(EV_Count) * 100)
# Summarize the findings
summary(ev_regional_analysis)
###4
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
# Analyze policies in high-adoption areas
# Extract data for high-adoption counties and their associated policies
ev_policies_high <- electric_vehicle_data %>%
  filter(county %in% top_5$county) %>%
  group_by(county, legislative_district, electric_utility) %>%
  summarise(EV_Count = n(), .groups = 'drop') %>%
  mutate(Infrastructure_Score = EV_Count / sum(EV_Count) * 100)
# Summarize the policies and infrastructure scores for high-adoption areas
summary(ev_policies_high)
# Plotting using geom_point for better visibility of points
ggplot(ev_geo_data, aes(x = long, y = lat, color = Adoption_Rate)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_viridis(name = "Adoption Rate (%)", option = "C") +
  labs(title = 'Simulated Map of EV Adoption Rates Across Different Regions', x = 'Longitude', y = 'Latitude') +
  theme_minimal()
# Plotting using geom_point for better visibility of points
ggplot(ev_geo_data, aes(x = long, y = lat, color = Adoption_Rate)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_viridis(name = "Adoption Rate (%)", option = "C") +
  labs(title = 'Simulated Map of EV Adoption Rates Across Different Regions', x = 'Longitude', y = 'Latitude') +
  theme_minimal()
###
# Using a scatter plot approach with color coding to simulate a choropleth map effect
library(ggplot2)
library(viridis)
# Create a fictional map data for counties
counties <- data.frame(county = unique(ev_geo_data$county), lat = runif(n = length(unique(ev_geo_data$county)), min = 34, max = 42), long = runif(n = length(unique(ev_geo_data$county)), min = -124, max = -114))
ev_geo_data <- merge(ev_geo_data, counties, by = 'county')
# Plotting using geom_point for better visibility of points
ggplot(ev_geo_data, aes(x = long, y = lat, color = Adoption_Rate)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_viridis(name = "Adoption Rate (%)", option = "C") +
  labs(title = 'Simulated Map of EV Adoption Rates Across Different Regions', x = 'Longitude', y = 'Latitude') +
  theme_minimal()
# Analyze regional policies and infrastructure affecting EV adoption
# Group data by legislative district and electric utility to infer policy and infrastructure influences
ev_regional_analysis <- electric_vehicle_data %>%
  group_by(legislative_district, electric_utility) %>%
  summarise(EV_Count = n(), .groups = 'drop') %>%
  mutate(Infrastructure_Score = EV_Count / sum(EV_Count) * 100)
# Summarize the findings
summary(ev_regional_analysis)
###
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
###5
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
# Generate a simple linear projection based on historical data
model <- lm(EV_Count ~ model_year, data = ev_trend_data)
future_years <- data.frame(model_year = seq(max(ev_trend_data$model_year) + 1, max(ev_trend_data$model_year) + 10))
projections <- predict(model, newdata = future_years)
# Develop a regression model to forecast future EV adoption trends
# Using the historical data prepared earlier
model <- lm(EV_Count ~ model_year, data = ev_trend_data)
# Summary of the regression model to understand the coefficients and accuracy
summary(model)
# A correlation matrix of the numeric values.
# Calculate the correlation matrix
correlation_matrix <- cor(electric_vehicle_data[, sapply(electric_vehicle_data, is.numeric)])
correlation_matrix
library(corrplot)
p_load(RColorBrewer)
corrplot(correlation_matrix, col = brewer.pal( n = 8, name = "RdYlBu"))
# Load necessary library
library(caTools)
# Splitting the data into training and testing sets
set.seed(123) # for reproducibility
split <- sample.split(EV_data_selected$Adoption, SplitRatio = 0.7)
train_data <- subset(EV_data_selected, split == TRUE)
test_data <- subset(EV_data_selected, split == FALSE)
# Training a logistic regression model
model <- glm(Adoption ~ ., family = binomial(link = 'logit'), data = train_data)
# Load the dataset
electric_vehicle_data <- read.csv('Electric_Vehicle.csv', stringsAsFactors = FALSE)
# Identify numerical and categorical variables in the dataset
classification <- sapply(electric_vehicle_data, class)
# Separate numerical and categorical variables based on their class
categorical_vars <- names(classification[classification %in% c('factor', 'character')])
numerical_vars <- names(classification[!(classification %in% c('factor', 'character'))])
# Print the lists of numerical and categorical variables
print(list('Categorical Variables' = categorical_vars, 'Numerical Variables' = numerical_vars))
# ===============Outliers================
# Load the dataset first
electric_vehicle_data <- read.csv('Updated_Electric_Vehicle.csv', stringsAsFactors = FALSE)
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
# ===============Outliers================
# Load the dataset first
electric_vehicle_data <- read.csv('Updated_Electric_Vehicle.csv', stringsAsFactors = FALSE)
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
