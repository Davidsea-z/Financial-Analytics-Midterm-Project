# Load required libraries
library(ExamPAData)
library(ggplot2)
library(dplyr)
library(pROC)

# Load auto_claim dataset
data(auto_claim)

# Convert character columns to factors
auto_claim$MAX_EDUC <- factor(auto_claim$MAX_EDUC)
auto_claim$GENDER <- factor(auto_claim$GENDER)

# Print first few rows of auto_claim dataset
print("First few rows of auto_claim dataset:")
print(head(auto_claim))

# a. Create scatter plot of CLM_AMT5 vs INCOME
scatter_plot <- ggplot(auto_claim, aes(x = INCOME, y = CLM_AMT5)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Claim Amount vs Income",
       x = "Income",
       y = "Claim Amount (5 years)") +
  theme(plot.title = element_text(hjust = 0.5))

# Save scatter plot
ggsave("q3_scatter_plot.png", scatter_plot, width = 8, height = 6)

# b. Fit linear regression model
linear_model <- lm(CLM_AMT5 ~ MAX_EDUC + AGE + GENDER, data = auto_claim)

# Create prediction data for PhD, Age 30, Female
pred_data <- data.frame(
  MAX_EDUC = factor("PhD", levels = levels(auto_claim$MAX_EDUC)),
  AGE = 30,
  GENDER = factor("F", levels = levels(auto_claim$GENDER))
)

# Get prediction with confidence interval
linear_pred <- predict(linear_model, newdata = pred_data, interval = "confidence", level = 0.95)

# Save linear model predictions
write.csv(data.frame(linear_pred), "q3_linear_model_ci.csv", row.names = FALSE)

# c. Fit logistic regression model
# Convert CLM_FREQ5 to binary (0 for no claims, 1 for at least one claim)
auto_claim$has_claim <- ifelse(auto_claim$CLM_FREQ5 > 0, 1, 0)

# Fit logistic model
logistic_model <- glm(has_claim ~ MAX_EDUC + AGE + GENDER, 
                     family = binomial(link = "logit"),
                     data = auto_claim)

# Get probability prediction for the same case
logistic_pred <- predict(logistic_model, newdata = pred_data, type = "response")

# Save logistic model prediction
write.csv(data.frame(probability = logistic_pred), "q3_logistic_pred.csv", row.names = FALSE)

# d. Generate ROC curve
roc_pred <- predict(logistic_model, type = "response")
roc_obj <- roc(auto_claim$has_claim, roc_pred)

# Create ROC plot
roc_plot <- ggroc(roc_obj) +
  theme_minimal() +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve for Claim Prediction Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  annotate("text", x = 0.75, y = 0.25,
           label = paste("AUC =", round(auc(roc_obj), 3))) +
  theme(plot.title = element_text(hjust = 0.5))

# Save ROC plot
ggsave("q3_roc_curve.png", roc_plot, width = 8, height = 6)

# Save model summaries
sink("q3_model_summaries.txt")
cat("Linear Regression Model Summary:\n")
print(summary(linear_model))
cat("\nLogistic Regression Model Summary:\n")
print(summary(logistic_model))
sink()