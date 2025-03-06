# Load necessary libraries
library(dplyr)

# Read the portfolio.rds file
portfolio_data <- readRDS("portfolio.rds")

# Calculate buy-and-hold return (B)
portfolio_data$B <- (portfolio_data$Close + portfolio_data$Div) / 
  (portfolio_data$Close - portfolio_data$CloseDifference) - 1

# Prepare data for ANOVA
strategy_returns <- data.frame(
  Return = c(portfolio_data$B, portfolio_data$M, portfolio_data$W),
  Strategy = factor(rep(c("B", "M", "W"), each = nrow(portfolio_data)))
)

# Conduct ANOVA test
anova_result <- aov(Return ~ Strategy, data = strategy_returns)

# Get summary statistics
summary_stats <- strategy_returns %>%
  group_by(Strategy) %>%
  summarise(
    Mean = mean(Return, na.rm = TRUE),
    SD = sd(Return, na.rm = TRUE),
    N = n()
  )

# Save results
write.csv(summary_stats, "q6_summary_stats.csv", row.names = FALSE)
write.csv(summary(anova_result)[[1]], "q6_anova_results.csv", row.names = FALSE)

# Print results
print("Summary Statistics:")
print(summary_stats)
print("\nANOVA Results:")
print(summary(anova_result))