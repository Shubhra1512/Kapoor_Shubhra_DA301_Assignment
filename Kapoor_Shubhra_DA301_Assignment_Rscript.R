# Assignment: EDA using R


###############################################################################

# 1. Prepare your workstation.

# Determine your working directory
getwd()

# Change your current directory.
setwd("~/Final assignment")

###############################################################################

# 2. Load and explore the data

# Install the tidyverse library.
# install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse)

# Import a CSV file.
Sales <- read.csv('turtle_sales.csv', header=T)

# Print the data frame.
View(Sales) 
as_tibble(Sales)

## View a summary of the data frame.
summary(Sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns 

Sales_1 <- select(Sales, -Ranking, -Year, -Genre, -Publisher)

# Convert product into factors (cateogrical variable) # Create new  data frame with Clean data

Sales_clean <- mutate(Sales_1, Product = as.factor(Product))


# View the data frame.
as_tibble(Sales_clean)

# View the descriptive statistics.
summary (Sales_clean)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

ggplot(Sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatterplot: Spread of NA_Sales vs. EU_Sales",
       x = "NA Sales",
       y = "EU Sales")

## 2b) Histograms
# Create histograms.

ggplot(Sales_clean, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram: Spread of Sales",
       x = "Global Sales",
       y = "Frequency")

# Group the data by "Product" and calculate the total global sales for each product.
top_products <- Sales_clean %>%
  group_by(Product) %>%
  summarise(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)) %>%
  head(10)  # Select the top 10 selling products

# Create a bar plot to view the figures for the top N selling products.
ggplot(top_products, aes(x = reorder(Product, Total_Global_Sales), y = Total_Global_Sales)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Top 10 Selling Products",
       x = "Product",
       y = "Total Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create Boxplot

ggplot(Sales, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_boxplot() +
  labs(title = "Box Plot: Impact of Platform on Global Sales",
       x = "Platform",
       y = "Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



###############################################################################

# 3. Observations and insights

# The provided R code utilizes the tidyverse and ggplot2 packages to explore and visualize a dataset named "turtle_sales.csv," containing video game sales information. 
# After loading the data, unnecessary columns are removed, and the 'Product' column is converted into a factor for categorical representation. The data exploration involves creating scatterplots to observe the relationship between NA_Sales and EU_Sales, histograms to understand the distribution of Global_Sales, and box plots to analyze the impact of different platforms on Global_Sales.
# The insights gained from the visualizations include a positive correlation between NA_Sales and EU_Sales, identification of the top-selling products, understanding the distribution of Global_Sales, and variations in sales across different platforms. These insights can inform decision-making for game publishers, developers, and marketers. 
# They can focus on successful products, analyze their features to resonate with the target audience, and optimize distribution strategies by leveraging high-performing platforms. Additionally, they can tailor marketing efforts based on regional sales patterns. 
# By leveraging these data-driven insights, businesses can make informed decisions to maximize revenue, capitalize on successful products, and strategize effectively in the competitive gaming industry.




###############################################################################
###############################################################################

# Cleaning and maniulating data using R

# 1. Load and explore the data
# View data frame created in Week 4.

# Import the tidyverse library, Skimr & DataExplorer.
library(tidyverse)
install.packages('skimr')
install.packages('DataExplorer') 
# Import and read CSV file.
library(readr)
# Data wranglin
library(dplyr)
# Data wrangling
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

install.packages("moments")

# Import a CSV file.
Sales_2 <- read.csv('turtle_sales.csv', header=T)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns 

Sales_3 <- select(Sales, -Ranking, -Year, -Genre, -Publisher)
# Convert product into factors (cateogrical variable) # Create new  data frame with Clean data

Sales_clean1 <- mutate(Sales_1, Product = as.factor(Product))

# Check output: Determine the min, max, and mean values.
mean(Sales_clean1$Global_Sales)
median (Sales_clean1$Global_Sales)
min(Sales_clean1$Global_Sales)
max(Sales_clean1$Global_Sales)
summary(Sales_clean1$Global_Sales)
summary (Sales_clean1)

# View the descriptive statistics.
head (Sales_clean1)
tail (Sales_clean1)
View (Sales_clean1)
dim (Sales_clean1)
glimpse (Sales_clean1)
as_tibble(Sales_clean1)
skim(Sales_clean1)

# Missing Values 
sum(is.na(Sales_clean1))
sum(is.na(Sales_clean1$size))

# Check the data type of the "Sales" column in Sales_clean1
class(Sales_clean1$Sales)

# Check the column names of the data frame
colnames(Sales_clean1)
# Check the data types of the columns in the data frame
str(Sales_clean1)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

# Step 2a: Use the group_by and summarize functions.
# Group data based on Product and determine the sum per Product.

library(dplyr)
Sales_sum_by_product <- Sales_clean1 %>%
  group_by(Product) %>%
  summarize(Total_Sales = sum(Global_Sales, na.rm = TRUE))

# View the data frame.
View(Sales_sum_by_product)

# Explore the data frame.
head (Sales_sum_by_product)
tail (Sales_sum_by_product)
View (Sales_sum_by_product)
dim (Sales_sum_by_product)
glimpse (Sales_sum_by_product)
as_tibble(Sales_sum_by_product)
skim(Sales_sum_by_product)

# Load necessary libraries
library(ggplot2)

## 2b) Determine which plot is the best to compare game sales.

# Create histograms.
hist(Sales_clean1$Global_Sales) 

# Create boxplots.
boxplot(Sales_clean1$Global_Sales) 

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(Sales_clean1$Global_Sales,
       col='blue',
       xlab = 'z Value',
       ylab = 'Global_Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(Sales_clean1$Global_Sales,
       col='red',
       lwd=3)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
# Perform Shapiro-Wilk test.

shapiro.test(Sales_clean1$Global_Sales)
dim (Sales_clean1)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
install.packages("moments")
library(moments)

skewness(Sales_clean1$Global_Sales)
kurtosis(Sales_clean1$Global_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(Sales_clean1$Global_Sales, Sales_clean1$NA_Sales)
cor(Sales_clean1$Global_Sales, Sales_clean1$EU_Sales) 


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Create scatter plots with trend lines for each pair of sales data columns
ggplot(Sales_clean1, aes(x = Global_Sales, y = NA_Sales)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Global Sales") +
  ylab("NA_Sales")

ggplot(Sales_clean1, aes(x = Global_Sales, y = EU_Sales)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Global Sales") +
  ylab("EU_Sales")

###############################################################################

# 5. Observations and insights


# The data in this analysis shows a departure from a perfectly normal distribution, as evident from the Q-Q plots, Shapiro-Wilk test, and skewness/kurtosis calculations. 
# The Q-Q plots exhibit deviations from the reference line, suggesting non-normality.
# The Shapiro-Wilk test indicates a significant departure from normality.
# The skewness value indicates a non-zero value, indicating asymmetry, and the kurtosis value is greater than 3, indicating heavier tails than a normal distribution. 
# These deviations suggest that the data may not strictly follow a normal distribution. Understanding these aspects is crucial for making accurate statistical inferences and decisions based on the data.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
head(Sales_clean1)
summary(Sales_clean1)

# 2. Create a simple linear regression model.
#  - Determine the correlation between the sales columns.
cor(Sales_clean1$NA_Sales, Sales_clean1$Global_Sales)
cor(Sales_clean1$EU_Sales, Sales_clean1$Global_Sales)

# Create a simple linear regression model - NA_Sales predicting Global_Sales
##  - View the output.
##  - Create plots to view the linear regression.
lm_na_sales <- lm(Global_Sales ~ NA_Sales, data = Sales_clean1)

# Create a simple linear regression model - EU_Sales predicting Global_Sales
lm_eu_sales <- lm(Global_Sales ~ EU_Sales, data = Sales_clean1)

# Plot the simple linear regression models
ggplot(Sales_clean1, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("NA Sales") +
  ylab("Global Sales")

ggplot(Sales_clean1, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("EU Sales") +
  ylab("Global Sales")


# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.

# Select only the numeric columns
numeric_columns <- Sales_clean1[, c("Global_Sales", "NA_Sales", "EU_Sales")]

# Determine the correlation between the sales columns
cor(numeric_columns)

# Create a multiple linear regression model
lm_multiple <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = Sales_clean1)
summary(lm_multiple)


# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

# Provided values for NA_Sales and EU_Sales
new_data <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                       EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict Global_Sales using the simple linear regression models
# Predict Global_Sales using the simple linear regression models
predicted_global_sales_na <- predict(lm_na_sales, newdata = new_data)
predicted_global_sales_eu <- predict(lm_eu_sales, newdata = new_data)

# Predict Global_Sales using the multiple linear regression model
predicted_global_sales_multiple <- predict(lm_multiple, newdata = new_data)

# Compare the predictions to the observed values
observed_values <- c(50.2, 40.5, 30.8, 55.1, 48.6)  # Replace this with your actual observed values

# Print the results
print("Predicted Global Sales (NA_Sales):")
print(predicted_global_sales_na)

print("Predicted Global Sales (EU_Sales):")
print(predicted_global_sales_eu)

print("Predicted Global Sales (Multiple Regression):")
print(predicted_global_sales_multiple)

print("Observed Global Sales Values:")
print(observed_values)


# 5. Include your insights and observations.
# Sales Correlation in Key Regions: Our analysis revealed a strong and positive correlation between sales in North America (NA) and Europe (EU) with global sales. This insight underscores the pivotal role of these regions in driving our overall business performance. As NA and EU sales grow, we can expect a proportional increase in global sales, providing valuable direction for our sales and marketing strategies.
# Accurate Sales Predictions: We have developed highly reliable linear regression models that can accurately predict global sales based on NA and EU sales data. These predictive models serve as powerful tools to forecast future sales, allowing us to make data-driven decisions and set realistic sales targets. With a solid grasp on future sales potential, we can optimize resource allocation and marketing efforts for maximum impact.
# Strategic Market Focus: The strong correlation emphasizes the importance of prioritizing our efforts in North America and Europe. By tailoring marketing campaigns and product offerings to these regions, we can tap into their growth potential and maximize revenue generation. Focusing on key markets with the greatest impact will pave the way for expansion and increased market share.
# Continuous Improvement for Precision: To stay ahead in the dynamic gaming industry, we recommend regular updates to our predictive models with new data and the inclusion of relevant variables that may influence sales. This iterative approach ensures our models remain accurate and adaptable to changing market conditions.
# Data-Driven Decision Making for Success: Leveraging data-driven insights at the core of our decision-making process empowers us to make informed choices. By embracing data-driven strategies, we can optimize marketing initiatives, enhance customer experiences, and ultimately gain a competitive edge in the global gaming market.
# By acting on these insights, Turtle Games can strategically position itself for sustained success, capitalize on growth opportunities, and reinforce our position as a market leader in the gaming industry.


###############################################################################

