# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load data from data_q2.csv
data <- read.csv("data_q2.csv")

# Summarize mean and standard deviation of returns for each day
summary_table <- data %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))))

# Print and save summary table
print(summary_table)
write.csv(summary_table, "q2_summary.csv", row.names = FALSE)

# Conduct ANOVA test
anova_result <- aov(cbind(Mon, Tue, Wed, Thu, Fri) ~ 1, data = data)
summary(anova_result)

# Save ANOVA results
write.csv(summary(anova_result)[[1]], "q2_anova_results.csv", row.names = FALSE)

# Predict Tuesday's return with 95% confidence interval
tuesday_pred <- predict(anova_result, newdata = data.frame(day = "Tue"), interval = "confidence")

# Print and save prediction results
print(tuesday_pred)
write.csv(tuesday_pred, "q2_tuesday_prediction.csv", row.names = FALSE)