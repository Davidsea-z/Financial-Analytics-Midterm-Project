# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the data
data <- read.csv("data_q1.csv")

# a. Create summary table
summary_table <- data %>%
  group_by(Strategy) %>%
  summarise(
    `N(Buy)` = n(),
    `Buy > 0` = sum(Return > 0),
    `Buy (average)` = mean(Return)
  )

# Save summary table
write.csv(summary_table, "q1_summary_table.csv", row.names = FALSE)

# b. Create box plot
boxplot <- ggplot(data, aes(x = Strategy, y = Return)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Returns by Strategy",
       x = "Strategy",
       y = "Return")

# Save box plot
ggsave("q1_boxplot.png", boxplot)

# c. Conduct ANOVA test
anova_model <- aov(Return ~ Strategy, data = data)
anova_results <- summary(anova_model)

# Calculate µ (overall average) and α_i values
overall_mean <- mean(data$Return)
alpha_values <- coef(anova_model)

# Create results dataframe
anova_params <- data.frame(
  Parameter = c("µ (overall mean)", "α₁ (Strategy A)"),
  Value = c(overall_mean, alpha_values[2])
)

# Save ANOVA results
write.csv(anova_results[[1]], "q1_anova_results.csv", row.names = TRUE)
write.csv(anova_params, "q1_anova_parameters.csv", row.names = FALSE)