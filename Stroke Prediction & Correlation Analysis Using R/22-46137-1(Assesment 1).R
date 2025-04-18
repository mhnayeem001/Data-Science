
library(readr)
library(dplyr)
library(tidyr) 
library(ggpubr)
library(ggcorrplot)
library(ggplot2)

stroke_data <- read.csv("E:/8th Sem/final data scnc/assesment 1/healthcare-dataset-stroke-data.csv")
View(stroke_data)

str(stroke_data)
summary(stroke_data)

missing_values <- colSums(is.na(stroke_data))
print(missing_values)

head(stroke_data, 20)

numeric_columns <- sapply(stroke_data, is.numeric)
print(numeric_columns)

numeric_column_names <- names(stroke_data)[numeric_columns]
print("Numeric columns are:")
print(numeric_column_names)




cor_matrix <- cor(stroke_data[, sapply(stroke_data, is.numeric)])
print(cor_matrix)


correlation_with_stroke <- cor(stroke_data$stroke, stroke_data %>% select_if(is.numeric))
print(correlation_with_stroke)





positive_corr <- cor_matrix[cor_matrix > 0]
print(positive_corr)

positive_corr_matrix <- cor_matrix
positive_corr_matrix[positive_corr_matrix <= 0] <- NA  
print(positive_corr_matrix)

negative_corr <- cor_matrix[cor_matrix < 0]
print(negative_corr)

negative_corr_matrix <- cor_matrix
negative_corr_matrix[negative_corr_matrix >= 0] <- NA  
print(negative_corr_matrix)

zero_corr_matrix <- cor_matrix
zero_corr_matrix[zero_corr_matrix != 0] <- NA  
print(zero_corr_matrix)

# Pearson Correlation test


correlation_test <- cor.test(stroke_data$age, stroke_data$stroke, method = "pearson")
print(correlation_test)


correlation_test <- cor.test(stroke_data$hypertension, stroke_data$stroke, method = "pearson")
print(correlation_test)


correlation_test <- cor.test(stroke_data$heart_disease, stroke_data$stroke, method = "pearson")
print(correlation_test)


correlation_test <- cor.test(stroke_data$avg_glucose_level, stroke_data$stroke, method = "pearson")
print(correlation_test)

stroke_data$bmi <- as.numeric(as.character(stroke_data$bmi))


correlation_test <- cor.test(stroke_data$bmi, stroke_data$stroke, method = "pearson")
print(correlation_test)

# Pearson Method Plots
ggscatter(stroke_data, x = "age", y = "stroke",
          add = "reg.line",
          conf.int = FALSE,
          cor.coef = TRUE,
          cor.method = "pearson",
          color = "red",
          size = 2,
          shape = 20,
          fill = "black") +
  theme_classic() +
  labs(x = "Age", y = "Stroke", title = "Correlation Plot of Age and Stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))



ggplot(stroke_data, aes(x = hypertension, y = stroke)) +
  geom_point(size = 2, shape = 20, color = "red", fill = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "pearson", label.x = 1, label.y = max(stroke_data$stroke)) +
  theme_classic() +
  labs(x = "Hypertension", y = "Stroke", title = "Correlation Plot of Hypertension and Stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))

ggplot(stroke_data, aes(x = heart_disease, y = stroke)) +
  geom_point(size = 2, shape = 20, color = "red", fill = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "pearson", label.x = 1, label.y = max(stroke_data$stroke)) +
  theme_classic() +
  labs(x = "Heart Disease", y = "Stroke", title = "Correlation Plot of Heart Disease and Stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))

ggplot(stroke_data, aes(x = avg_glucose_level, y = stroke)) +
  geom_point(size = 2, shape = 20, color = "red", fill = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "pearson", label.x = 1, label.y = max(stroke_data$stroke)) +
  theme_classic() +
  labs(x = "Avg Glucose Level", y = "Stroke", title = "Correlation Plot of Avg Glucose Level and Stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))

ggplot(stroke_data, aes(x = bmi, y = stroke)) +
  geom_point(size = 2, shape = 20, color = "red", fill = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "pearson", label.x = 1, label.y = max(stroke_data$stroke)) +
  theme_classic() +
  labs(x = "BMI", y = "Stroke", title = "Correlation Plot of BMI and Stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))

# Spearman Correlation test


suppressWarnings({
  correlation_test <- cor.test(as.numeric(stroke_data$age), stroke_data$stroke, method = "spearman")
})
print(correlation_test)


correlation_test <- cor.test(as.numeric(stroke_data$avg_glucose_level), stroke_data$stroke, method = "spearman")
print(correlation_test)



spearman_correlation <- cor(as.numeric(stroke_data$smoking_status), stroke_data$stroke, method = "spearman")
print(spearman_correlation)


# Spearman Method Plots



ggplot(stroke_data, aes(x = avg_glucose_level, y = stroke)) +
  geom_point(size = 2, shape = 20, color = "red", fill = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "spearman", label.x = 1, label.y = max(stroke_data$stroke)) +
  theme_classic() +
  labs(x = "avg_glucose_level", y = "stroke", title = "Correlation Plot of avg_glucose_level and stroke") +
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12))






clean_stroke_data <- stroke_data %>%
  filter(!is.na(age) & !is.na(bmi) & is.finite(age) & is.finite(bmi)) %>%
  mutate(bmi = as.numeric(bmi)) %>%  
  filter(!is.na(bmi))  

# Ensure clean_stroke_data has non-empty BMI data
if (nrow(clean_stroke_data) > 0) {
  # Plot with a linear regression line and Spearman correlation
  ggplot(clean_stroke_data, aes(x = age, y = bmi)) +
    geom_point(size = 2, shape = 20, color = "red", fill = "black") +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Linear model for fit
    stat_cor(method = "spearman", label.x = 1, label.y = max(clean_stroke_data$bmi, na.rm = TRUE) - 0.5) +  # Adjust label position
    theme_classic() +
    labs(x = "Age", y = "BMI", title = "Correlation Plot of Age and BMI") +
    theme(axis.title = element_text(size = 12), 
          axis.text = element_text(size = 12))
} else {
  print("No valid data available for plotting.")
}







categorical_columns <- sapply(stroke_data, function(col) is.factor(col) || is.character(col))
print(categorical_columns)

categorical_column_names <- names(stroke_data)[categorical_columns]
print(categorical_column_names)


contingency_table <- table(stroke_data$work_type, stroke_data$Residence_type)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

contingency_table <- table(stroke_data$gender, stroke_data$smoking_status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)





clean_stroke_data <- stroke_data %>%
  select_if(is.numeric) %>%  
  na.omit()      


correlation_matrix_pearson <- cor(clean_stroke_data, method = "pearson")
correlation_matrix_spearman <- cor(clean_stroke_data, method = "spearman")

# Heatmap
ggcorrplot(correlation_matrix_pearson, 
           method = "square", 
           type = "lower",     
           lab = TRUE,        
           title = "Pearson Correlation Heatmap for Stroke Data",
           lab_size = 2,       
           colors = c("red", "white", "blue"),  
           ggtheme = ggplot2::theme_minimal())


ggcorrplot(correlation_matrix_spearman, 
           method = "circle", 
           type = "upper",     
           lab = TRUE,         
           title = "Spearman Correlation Heatmap for Stroke Data",
           lab_size = 2,       
           colors = c("red", "white", "blue"),  
           ggtheme = theme_minimal())

stacked_table <- stroke_data %>%
  group_by(gender, hypertension, heart_disease, smoking_status, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0)

print(stacked_table)


stacked_table <- stroke_data %>%
  group_by(gender, hypertension, heart_disease, smoking_status, stroke) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(stacked_table, aes(x = gender, y = count, fill = factor(stroke))) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Stroke Cases by Gender",
       x = "Gender",
       y = "Count",
       fill = "Stroke") +
  theme_minimal()


# Scatter Plots

plot_scatter <- function(data, x_col, y_col, color = "blue") {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(color = color, alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed") +
    labs(title = paste("Scatter Plot of", x_col, "vs", y_col),
         x = x_col, y = y_col) +
    theme_minimal()
}

stroke_data_subset <- head(stroke_data, 500)

scatter_age_bmi <- plot_scatter(stroke_data_subset, "age", "bmi", color = "blue")
scatter_age_glucose <- plot_scatter(stroke_data_subset, "age", "avg_glucose_level", color = "green")
scatter_bmi_glucose <- plot_scatter(stroke_data_subset, "bmi", "avg_glucose_level", color = "purple")
scatter_age_heart <- plot_scatter(stroke_data_subset, "age", "heart_disease", color = "red")

print(scatter_age_bmi)
print(scatter_bmi_glucose)
print(scatter_age_heart)
print(scatter_age_glucose)






