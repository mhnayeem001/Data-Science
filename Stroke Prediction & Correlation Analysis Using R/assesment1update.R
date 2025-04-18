# Load necessary libraries
library(readr)
library(dplyr)
library(ggpubr)
library(ggcorrplot)
library(ggplot2)
library(tidyr) 


# Load the stroke dataset (adjust the path to your file)
stroke_data <- read.csv("E:/8th Sem/final data scnc/assesment 1/healthcare-dataset-stroke-data.csv")
View(stroke_data)

# Check the structure and summary of the dataset
str(stroke_data)
summary(stroke_data)




# Check for missing values
missing_values <- colSums(is.na(stroke_data))
print(missing_values)

# Find numerical and categorical columns
numeric_columns <- sapply(stroke_data, is.numeric)
categorical_columns <- sapply(stroke_data, is.character) | sapply(stroke_data, is.factor)

# Print the numeric column names
numeric_column_names <- names(stroke_data)[numeric_columns]
print("Numeric columns are:")
print(numeric_column_names)

# Print the categorical column names
categorical_column_names <- names(stroke_data)[categorical_columns]
print("Categorical columns are:")
print(categorical_column_names)


####histogram of all numeric value

# Select numerical columns
numeric_columns <- stroke_data_clean %>% select(age, avg_glucose_level, hypertension, heart_disease) 

# Reshape the data to long format
long_data <- pivot_longer(numeric_columns, cols = everything(), names_to = "Variable", values_to = "Value")

# Create improved combined histogram
ggplot(long_data, aes(x = Value, fill = Variable)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +  # Create separate panels for each variable
  labs(title = "Histograms of Numerical Columns",
       x = "Value",
       y = "Frequency",
       fill = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")  # Use a color palette for better visualization


####CHECK THE NORMALLITTY


# Select the first 2000 entries
stroke_data_subset <- stroke_data_clean[1:2000, ]

# Select numerical columns from the subset
numeric_columns <- stroke_data_subset %>% select(age, avg_glucose_level, hypertension, heart_disease)  # Adjust as needed

# Reshape the data to long format
long_data <- pivot_longer(numeric_columns, cols = everything(), names_to = "Variable", values_to = "Value")

# Initialize results data frame
normality_results <- data.frame(Variable = character(),
                                W = numeric(),
                                p_value = numeric(),
                                Normal_Distribution = character(),
                                stringsAsFactors = FALSE)

# Check normality for each numerical column in the subset
for (column in colnames(numeric_columns)) {
  shapiro_test <- shapiro.test(numeric_columns[[column]])
  
  # Store results
  normality_results <- rbind(normality_results, 
                             data.frame(Variable = column,
                                        W = shapiro_test$statistic,
                                        p_value = shapiro_test$p.value,
                                        Normal_Distribution = ifelse(shapiro_test$p.value > 0.05, "Yes", "No"),
                                        stringsAsFactors = FALSE))
}

# Print normality results
print(normality_results)

###CHECK THE SKEWNESSS

if (!require(moments)) {
  install.packages("moments", dependencies = TRUE)
}

library(moments)


# Calculate skewness for each variable
skewness_results <- sapply(numeric_columns, skewness)

# Create a data frame to store skewness results
skewness_summary <- data.frame(
  Variable = names(skewness_results),
  Skewness = skewness_results,
  Skewness_Type = ifelse(skewness_results > 0, "Positive", 
                         ifelse(skewness_results < 0, "Negative", "Zero"))
)

# Print skewness summary
print(skewness_summary)


#GRAPH OF ALL SKEWNWESS


# Visualization of skewness
ggplot(skewness_summary, aes(x = Variable, y = Skewness, fill = Skewness_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Skewness of Numerical Variables",
       x = "Variable",
       y = "Skewness",
       fill = "Skewness Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "lightblue", "Negative" = "salmon", "Zero" = "gray")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")  # Add a horizontal line at y = 0


###MEAN MODE MEDIAN OF SKEWED VARIABLE



# Function to calculate mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Initialize a data frame to store mean, median, and mode
summary_stats <- data.frame(
  Variable = names(numeric_columns),
  Mean = sapply(numeric_columns, mean),
  Median = sapply(numeric_columns, median),
  Mode = sapply(numeric_columns, get_mode)
)

# Print summary statistics
print(summary_stats)




# Define a function to create and print the histogram
create_histogram <- function(var, summary_stats) {
  mean_value <- summary_stats$Mean[var]
  median_value <- summary_stats$Median[var]
  mode_value <- summary_stats$Mode[var]
  
  p <- ggplot(numeric_columns, aes_string(x = var)) +
    geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = median_value, color = "green", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = mode_value, color = "yellow", linetype = "dashed", linewidth = 1) +
    labs(title = paste(var, "Histogram with Skewness"),
         x = var,
         y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "top") +
    annotate("text", x = mean_value, y = max(numeric_columns[[var]], na.rm = TRUE), label = "Mean", color = "red", vjust = -1) +
    annotate("text", x = median_value, y = max(numeric_columns[[var]], na.rm = TRUE), label = "Median", color = "green", vjust = -1) +
    annotate("text", x = mode_value, y = max(numeric_columns[[var]], na.rm = TRUE), label = "Mode", color = "yellow", vjust = -1)
  
  # Print the plot
  print(p)
}

stroke_data_clean <- stroke_data %>%
  na.omit()

# Create histograms for "age" and "avg_glucose_level"
create_histogram("age", summary_stats)
create_histogram("avg_glucose_level", summary_stats)



# Example data: Replace 'numeric_column' with your actual column name
numeric_column <- stroke_data_clean$age  # Use your dataset and variable

# Create the histogram with a line plot
ggplot(stroke_data_clean, aes(x = numeric_column)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = NA, color = "steelblue", size = 1) +
  geom_density(color = "orange", size = 1.2) +  # Overlay line plot for density (modification to histogram)
  labs(title = "Line Histogram for Numerical Variable",
       x = "X axis",
       y = "Y axis") +
  theme_minimal()


# Bar plot for 'gender'
ggplot(stroke_data_clean, aes(x = gender)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Bar Plot for Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()

# Bar plot for 'ever_married'
ggplot(stroke_data_clean, aes(x = ever_married)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Bar Plot for Ever Married",
       x = "Ever Married",
       y = "Count") +
  theme_minimal()

# Bar plot for 'work_type'
ggplot(stroke_data_clean, aes(x = work_type)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Bar Plot for Work Type",
       x = "Work Type",
       y = "Count") +
  theme_minimal()

# Bar plot for 'Residence_type'
ggplot(stroke_data_clean, aes(x = Residence_type)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Bar Plot for Residence Type",
       x = "Residence Type",
       y = "Count") +
  theme_minimal()


# Bar plot for 'smoking_status'
ggplot(stroke_data_clean, aes(x = smoking_status)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot for Smoking Status",
       x = "Smoking Status",
       y = "Count") +
  theme_minimal()



# Box plot for 'age'
ggplot(stroke_data_clean, aes(y = age)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot for Age",
       y = "Age") +
  theme_minimal()

# Box plot for 'avg_glucose_level'
ggplot(stroke_data_clean, aes(y = avg_glucose_level)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Box Plot for Average Glucose Level",
       y = "Average Glucose Level") +
  theme_minimal()








install.packages("GGally")  # Uncomment if GGally is not installed
library(GGally)

# Load your dataset (replace with your actual file path)
# stroke_data <- read.csv("path_to_your_stroke_data.csv")

# Example dataset (for demonstration purposes)
# stroke_data <- data.frame(
#   Age = c(60, 70, 80, NA, 65),
#   BMI = c(25, 30, 32, 28, 27),
#   BloodPressure = c(120, 130, 140, 135, NA),
#   Diagnosis = factor(c("No", "Yes", "Yes", "No", "No"))
# )

# Remove rows with NA values
stroke_data_clean <- na.omit(stroke_data)

# Select only numeric columns
numeric_columns <- sapply(stroke_data_clean, is.numeric)
stroke_data_numeric <- stroke_data_clean[, numeric_columns]

# Create a scatter matrix using pairs()
pairs(stroke_data_numeric, 
      main = "Scatter Matrix of Stroke Dataset", 
      pch = 19)

# Create a scatter matrix using GGally
ggpairs(stroke_data_clean, 
        aes(color = Diagnosis),  # Adjust 'Diagnosis' if needed
        title = "Scatter Matrix of Stroke Dataset")

install.packages("GGally")


# Load necessary libraries
library(GGally)


# Select the numeric columns excluding 'bmi'
numeric_cols <- stroke_data[, c("age", "avg_glucose_level", "hypertension", "heart_disease")]

# Create a new column to factor stroke values for coloring
stroke_data$stroke <- as.factor(stroke_data$stroke)

# Create a scatter plot matrix without 'bmi'
ggpairs(numeric_cols, 
        aes(color = stroke_data$stroke, alpha = 0.6),
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor")),
        diag = list(continuous = wrap("densityDiag"))) +
  scale_color_manual(values = c("blue", "red")) + # Stroke = 0 (blue), Stroke = 1 (red)
  theme_minimal() +
  ggtitle("Scatter Matrix of Numeric Features (Color-Coded by Stroke)") +
  theme(plot.title = element_text(hjust = 0.5))




