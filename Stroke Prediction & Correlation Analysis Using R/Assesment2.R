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


stroke_data_clean <- stroke_data %>%
  select(age, avg_glucose_level, hypertension, heart_disease) %>%
  drop_na()

# Reshape the data to long format
long_data <- pivot_longer(stroke_data_clean, cols = everything(), names_to = "Variable", values_to = "Value")


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



###Histogram with skewness

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



# Create a new variable for age groups
stroke_data_clean$age_group <- cut(stroke_data_clean$age, breaks = seq(0, 100, by = 10), right = FALSE)

# Create a stacked bar chart
ggplot(stroke_data_clean, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Stacked Bar Chart for Age Groups",
       x = "Age Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")  # Optional: Use a color palette for better visualization


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





# Load necessary libraries
if (!require(ggplot2)) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}
if (!require(tidyr)) {
  install.packages("tidyr", dependencies = TRUE)
  library(tidyr)
}

# Load your dataset (replace with the actual path of your dataset)
# stroke_data <- read.csv("path_to_your_stroke_data.csv")

# Convert the categorical column to factor if it's not already
stroke_data$stroke <- as.factor(stroke_data$stroke)

# Reshape the data to long format, excluding BMI
stroke_long <- pivot_longer(stroke_data, 
                            cols = c(age, avg_glucose_level), 
                            names_to = "Variable", 
                            values_to = "Value")

# Create the violin plot with outliers, mean, median, and mode
p <- ggplot(stroke_long, aes(x = stroke, y = Value, fill = Variable)) + 
  geom_violin(trim = FALSE, color = "black") +  
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Boxplot without hiding outliers
  labs(title = "Violin Plot of Age and Avg Glucose Level by Stroke Outcome",
       x = "Stroke Outcome",
       y = "Value") +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +  # Change color palette
  theme(legend.title = element_blank())

# Add mean, median, and mode points
p <- p + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "blue") +  # Mean points
  stat_summary(fun = median, geom = "point", size = 2, color = "red") +  # Median points
  stat_summary(fun = function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]  # Calculate mode
  }, geom = "point", size = 2, color = "green", shape = 17)  # Mode points

# Print the updated violin plot
print(p)





# Load necessary packages
if (!require(fmsb)) {
  install.packages("fmsb")
  library(fmsb)
}

# Load your dataset (replace with the actual path)
# stroke_data <- read.csv("path_to_your_stroke_data.csv")

# Ensure to remove any rows with NA values
stroke_data <- na.omit(stroke_data)

# Calculate average values for numeric attributes (excluding BMI)
average_values <- data.frame(
  Variable = c("Age", "Hypertension", "Heart Disease", "Avg Glucose Level"),
  Stroke = c(mean(stroke_data$age[stroke_data$stroke == 1], na.rm = TRUE),
             mean(stroke_data$hypertension[stroke_data$stroke == 1], na.rm = TRUE),
             mean(stroke_data$heart_disease[stroke_data$stroke == 1], na.rm = TRUE),
             mean(stroke_data$avg_glucose_level[stroke_data$stroke == 1], na.rm = TRUE)),
  No_Stroke = c(mean(stroke_data$age[stroke_data$stroke == 0], na.rm = TRUE),
                mean(stroke_data$hypertension[stroke_data$stroke == 0], na.rm = TRUE),
                mean(stroke_data$heart_disease[stroke_data$stroke == 0], na.rm = TRUE),
                mean(stroke_data$avg_glucose_level[stroke_data$stroke == 0], na.rm = TRUE))
)

# Prepare the radar chart data frame format
# Corrected to avoid list coercion issues
radar_data <- as.data.frame(rbind(
  rep(500, 4),  # Maximum values
  rep(0, 4),    # Minimum values
  t(average_values[, -1])  # Transpose to get numeric data for Stroke and No_Stroke
))

# Set row names for clarity
rownames(radar_data) <- c("Max", "Min", "Stroke", "No Stroke")

# Create the radar chart
radarchart(radar_data, axistype = 1,
           pcol = c("red", "blue"),  # Colors for Stroke and No Stroke
           pfcol = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),  # Fill colors with transparency
           plwd = 2, plty = 1,
           title = "Comparison of Numeric Attributes: Stroke vs. No Stroke ")






# Load necessary libraries
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Assuming 'stroke_data' is already loaded in your environment
# Check column names to ensure 'age' and 'avg_glucose_level' exist
print(colnames(stroke_data))

# Subset the first 100 rows of the dataset
stroke_data_subset <- stroke_data %>%
  slice(1:20) %>%  # Select the first 100 rows
  filter(!is.na(age) & !is.na(avg_glucose_level))  # Remove rows with NA values in age or avg_glucose_level

# Create a line graph to show how 'avg_glucose_level' changes with 'age' for the first 100 rows
line_plot <- ggplot(stroke_data_subset, aes(x = age, y = avg_glucose_level)) +
  geom_line(color = "blue", size = 1, alpha = 0.7) +  # Line graph showing trend
  geom_point(color = "darkblue", size = 2, alpha = 0.6) +  # Add points to indicate each observation
  labs(title = "Line Graph of Avg Glucose Level by Age ",
       x = "Age (years)",
       y = "Avg Glucose Level",
       caption = "Source: Stroke Dataset ") +
  theme_minimal() +  # Clean minimal theme for better focus on the data
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Display the line graph
print(line_plot)

# Save the plot to a file
ggsave("line_graph_stroke_data.png", line_plot, width = 10, height = 6)












if (!require(GGally)) {
  install.packages("GGally")
  library(GGally)
}

# Remove rows with NA values
stroke_data_clean <- na.omit(stroke_data)

# Randomly select 100 rows from the cleaned dataset
set.seed(123)  # For reproducibility
sampled_indices <- sample(1:nrow(stroke_data_clean), 100)
sampled_data <- stroke_data_clean[sampled_indices, ]

# Factor the stroke column for coloring
sampled_data$stroke <- as.factor(sampled_data$stroke)

# Select the numeric columns for the scatter matrix (excluding 'bmi')
numeric_cols <- sampled_data[, c("age", "avg_glucose_level", "hypertension", "heart_disease")]

# Remove constant columns before using ggpairs
numeric_cols <- numeric_cols[, sapply(numeric_cols, sd) > 0]

# Ensure that sampled_data only includes columns that have variance
sampled_data <- sampled_data[, c("stroke", names(numeric_cols))]

# Suppress warnings and create a scatter plot matrix, color-coded by stroke
suppressWarnings({
  Scatter_Matrix <- ggpairs(sampled_data,
                            columns = names(numeric_cols),  # Use filtered numeric columns
                            aes(color = stroke),
                            lower = list(continuous = "points"),
                            diag = list(continuous = "densityDiag"),
                            upper = list(continuous = "cor"))
})

# Print the scatter matrix
print(Scatter_Matrix)









# Install and load the rgl package
if (!require(rgl)) {
  install.packages("rgl")
  library(rgl)
}

# Convert stroke column to factor
stroke_data$stroke <- as.factor(stroke_data$stroke)

# Clean the dataset by removing rows with NA values
stroke_data_clean <- na.omit(stroke_data)

# Select only the first 100 rows
stroke_data_subset <- stroke_data_clean[1:100, ]

# Create a 3D scatter plot
plot3d(stroke_data_subset$age, 
       stroke_data_subset$avg_glucose_level, 
       stroke_data_subset$hypertension, 
       col = as.numeric(stroke_data_subset$stroke),  # Color by stroke outcome
       size = 3, 
       xlab = "Age", 
       ylab = "Average Glucose Level", 
       zlab = "Hypertension")











# Install and load the rgl package
if (!require(rgl)) {
  install.packages("rgl")
  library(rgl)
}

# Convert stroke column to factor
stroke_data$stroke <- as.factor(stroke_data$stroke)

# Clean the dataset by removing rows with NA values
stroke_data_clean <- na.omit(stroke_data)

# Select only the first 100 rows
stroke_data_subset <- stroke_data_clean[1:100, ]

# Create a grid of values for the wireframe
age_seq <- seq(min(stroke_data_subset$age), max(stroke_data_subset$age), length.out = 30)
glucose_seq <- seq(min(stroke_data_subset$avg_glucose_level), max(stroke_data_subset$avg_glucose_level), length.out = 30)
z_matrix <- outer(age_seq, glucose_seq, function(age, glucose) {
  # Create a linear model or any other model to predict hypertension
  predict(lm(hypertension ~ age + avg_glucose_level, data = stroke_data_subset), newdata = data.frame(age, avg_glucose_level = glucose))
})

# Create the 3D wireframe plot
rgl::persp3d(x = age_seq, 
             y = glucose_seq, 
             z = z_matrix, 
             col = "lightblue", 
             alpha = 0.5, 
             xlab = "Age", 
             ylab = "Average Glucose Level", 
             zlab = "Hypertension", 
             theta = 30, 
             phi = 30)














# Install and load ggplot2 package
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Convert stroke column to factor
stroke_data$stroke <- as.factor(stroke_data$stroke)

# Clean the dataset by removing rows with NA values
stroke_data_clean <- na.omit(stroke_data)

# Create a kernel density plot for age and average glucose level
ggplot(stroke_data_clean, aes(x = age, fill = stroke)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Kernel Density Plot of Age by Stroke Outcome",
       x = "Age",
       y = "Density") +
  theme_minimal()

# Kernel density plot for average glucose level
ggplot(stroke_data_clean, aes(x = avg_glucose_level, fill = stroke)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Kernel Density Plot of Average Glucose Level by Stroke Outcome",
       x = "Average Glucose Level",
       y = "Density") +
  theme_minimal()








# Install and load ggplot2 package
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load the dataset and remove NA values
stroke_data_clean <- na.omit(stroke_data)

# Select a subset of the data (first 100 rows)
stroke_data_subset <- stroke_data_clean[1:100, ]

# Create the bubble chart with separate plots for stroke outcomes
ggplot(stroke_data_subset, aes(x = age, 
                               y = avg_glucose_level, 
                               size = hypertension, 
                               color = stroke)) +
  geom_point(alpha = 0.7) +  # Use geom_point to create the bubbles
  scale_size(range = c(1, 10), name = "Hypertension Level") +  # Adjust bubble size range
  labs(title = "Bubble Chart of Stroke Data",
       x = "Age",
       y = "Average Glucose Level",
       color = "Stroke Outcome") +
  
  theme_minimal()



  


