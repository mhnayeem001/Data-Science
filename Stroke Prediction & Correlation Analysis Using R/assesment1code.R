# Load necessary libraries
library(readr)
library(dplyr)
library(ggpubr)

# Load the stroke dataset (adjust the path to your file)
stroke_data <- read.csv("E:/8th Sem/final data scnc/assesment 1/healthcare-dataset-stroke-data.csv")
View(stroke_data)


summary(stroke_data)


# View the structure of the dataset
str(stroke_data)




# Check for missing values
missing_values <- colSums(is.na(stroke_data))
print(missing_values)

# Display the first few rows
head(stroke_data, 6)

#jhamela kortasilo

stroke_data$bmi <- as.numeric(as.character(stroke_data$bmi))

stroke_data$stroke <- as.numeric(as.character(stroke_data$stroke))


# Calculate Pearson correlation between numeric variables and Stroke outcome
pearson_correlation <- cor(stroke_data$age, stroke_data$stroke, method = "pearson")
print(pearson_correlation)

pearson_correlation <- cor(stroke_data$hypertension, stroke_data$stroke, method = "pearson")
print(pearson_correlation)

pearson_correlation <- cor(stroke_data$heart_disease, stroke_data$stroke, method = "pearson")
print(pearson_correlation)

pearson_correlation <- cor(stroke_data$avg_glucose_level, stroke_data$stroke, method = "pearson")
print(pearson_correlation)



pearson_correlation_bmi <- cor(stroke_data$bmi, stroke_data$stroke, method = "pearson")
print(paste("Pearson Correlation between BMI and Stroke:", pearson_correlation_bmi))

# Spearman correlation between smoking_status and stroke
spearman_correlation <- cor(as.numeric(stroke_data$smoking_status), stroke_data$stroke, method = "spearman")
print(spearman_correlation)

# Install ggpubr if not already installed
if (!require(ggpubr)) {
  install.packages("ggpubr")
}

# Load ggpubr library
library(ggpubr)

# Visualize correlation using ggscatter plots
ggscatter(stroke_data, x = "age", y = "stroke", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Stroke")
ggscatter(stroke_data, x = "avg_glucose_level", y = "stroke", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Avg Glucose Level", ylab = "Stroke")

ggscatter(stroke_data, x = "bmi", y = "stroke", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "BMI", ylab = "Stroke")

# ANOVA to find significant features in the stroke dataset
anova_analysis <- lapply(stroke_data %>% select(where(is.numeric)), function(feature) {
  # Perform ANOVA test for each numeric feature against the stroke outcome
  anova_result <- aov(feature ~ stroke_data$stroke)  # Stroke as a factor
  return(summary(anova_result))
})

# Extract p-values from the ANOVA results
p_values <- sapply(anova_analysis, function(result) {
  result[[1]]$"Pr(>F)"[1]  # Extract p-value from the summary result
})

# Create a summary data frame with feature names and p-values
anova_summary <- data.frame(
  Feature = names(stroke_data %>% select(where(is.numeric))),  # Feature names
  P_Value = unlist(p_values),
  row.names = NULL
)

# Filter significant features with p-value < 0.05
significant_features <- anova_summary %>% filter(P_Value < 0.05)
print(significant_features)

# Filter significant features with p-value < 0.05
significant_features <- anova_summary %>% filter(P_Value < 0.05)
print(significant_features)
