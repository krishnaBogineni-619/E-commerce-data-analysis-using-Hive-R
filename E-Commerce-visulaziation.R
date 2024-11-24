library(ggplot2)
library(dplyr)
library(lubridate)



# Load the dataset from a CSV file
project1_df <- read.csv("~/Downloads/project1_df.csv")

# Open the data in a spreadsheet-style viewer to inspect its structure
View(project1_df)

# Re-load the dataset into a variable named purchases_data for further use
purchases_data <- read.csv("~/Downloads/project1_df.csv")

# Display the column names in the dataset
colnames(purchases_data)

# Create a bar chart to show the count of purchases by gender
ggplot(purchases_data, aes(x = Gender)) +
  geom_bar(fill = "skyblue") + # Use sky blue bars
  labs(title = "Gender Distribution of Purchases", x = "Gender", y = "Count") + # Add titles and labels
  theme_minimal() # Apply a minimal theme for a clean look

# Create a bar chart to show the count of purchases by method
ggplot(purchases_data, aes(x = Purchase.Method)) +
  geom_bar(fill = "lightgreen") +  # Use light green bars
  labs(title = "Purchase Method Distribution", x = "Purchase Method", y = "Count") +  # Add titles and labels
  theme_minimal() +  # Apply a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 10))  # Adjust x-axis text
  )


library(dplyr)

# Load the lubridate package for working with date and time data
library(lubridate)

# Add new columns to represent the date, day of the week, and month
purchases_data <- purchases_data %>%
  mutate(
    Purchase.Date = as.Date(Purchase.Date),          # Convert the date column to Date format
    Day_Of_Week = wday(Purchase.Date, label = TRUE), # Extract the day of the week as a labeled factor
    Month = month(Purchase.Date, label = TRUE)       # Extract the month as a labeled factor
  )

# Summarize gross amount by day of the week and month
heatmap_data <- purchases_data %>%
  group_by(Day_Of_Week, Month) %>% # Group by day of the week and month
  summarise(Total_Gross = sum(Gross.Amount, na.rm = TRUE), .groups = "drop") # Calculate the total gross amount

# Create a heatmap showing gross amount distribution by day of the week and month
ggplot(heatmap_data, aes(x = Month, y = Day_Of_Week, fill = Total_Gross)) +
  geom_tile(color = "white") + # Create tiles with white borders
  labs(title = "Gross Amount by Day of the Week and Month", x = "Month", y = "Day of the Week") + # Add titles and labels
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Use a gradient color scale
  theme_minimal() # Apply a minimal theme

# Add a column for the year extracted from the Purchase.Date column
purchases_data <- purchases_data %>%
  mutate(Year = year(as.Date(Purchase.Date, format = "%Y-%m-%d"))) # Ensure the correct date format

# Summarize total sales by year and product category
annual_sales_by_category <- purchases_data %>%
  group_by(Year, Product.Category) %>% # Group by year and product category
  summarise(Total_Sales = sum(Net.Amount, na.rm = TRUE)) # Calculate total sales

# Create a stacked bar chart showing yearly sales by product category
ggplot(annual_sales_by_category, aes(x = Year, y = Total_Sales, fill = Product.Category)) +
  geom_bar(stat = "identity", position = "stack") + # Use a stacked bar chart
  labs(title = "Sales Contribution by Product Category per Year", x = "Year", y = "Total Sales (INR)", fill = "Product Category") + # Add titles and labels
  theme_minimal() + # Apply a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Summarize total sales by location
location_performance <- purchases_data %>%
  group_by(Location) %>% # Group by location
  summarise(Total_Sales = sum(Net.Amount, na.rm = TRUE)) # Calculate total sales


# Create a bar chart showing total sales by location
ggplot(location_performance, aes(x = Location, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "purple") + # Use purple bars
  labs(title = "Location-Based Sales Performance", x = "Location", y = "Total Sales (INR)") + # Add titles and labels
  theme_minimal() + # Apply a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Find the top 5 product categories based on total net amount
top_categories <- purchases_data %>%
  group_by(Product.Category) %>% # Group by product category
  summarise(Total_Net_Amount = sum(Net.Amount, na.rm = TRUE)) %>% # Calculate total net amount
  top_n(5, Total_Net_Amount) # Select the top 5 categories

# Create a bar chart showing the top 5 product categories by net amount
ggplot(top_categories, aes(x = reorder(Product.Category, -Total_Net_Amount), y = Total_Net_Amount)) +
  geom_bar(stat = "identity", fill = "orange") + # Use orange bars
  labs(title = "Top 5 Product Categories by Net Amount", x = "Product Category", y = "Total Net Amount (INR)") + # Add titles and labels
  theme_minimal() + # Apply a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Count purchases by method
method_distribution <- purchases_data %>%
  group_by(Purchase.Method) %>% # Group by purchase method
  summarise(Count = n()) # Count the number of occurrences

# Create a pie chart showing the distribution of purchase methods
ggplot(method_distribution, aes(x = "", y = Count, fill = Purchase.Method)) +
  geom_bar(width = 1, stat = "identity") + # Create a single bar (pie slice)
  coord_polar(theta = "y") + # Convert bar chart into a pie chart
  labs(title = "Distribution of Purchase Methods") + # Add a title
  theme_void() # Use an empty theme

# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

# Select numeric columns for correlation analysis
numeric_data <- purchases_data[, c("Gross.Amount", "Net.Amount", "Discount.Amount..INR.")]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Visualize the correlation matrix with a heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8) # Use color tiles with coefficients



