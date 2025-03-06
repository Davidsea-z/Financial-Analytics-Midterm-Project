# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the cash flow data
cash_flows <- read.csv("hsbc2.csv")

# Convert dates and calculate days from first cash flow
cash_flows$Date <- as.Date(cash_flows$Date)
cash_flows$Days <- as.numeric(cash_flows$Date - min(cash_flows$Date))

# Function to calculate NPV
calculate_npv <- function(rate, cash_flows) {
  sum(cash_flows$CashFlow / (1 + rate)^cash_flows$Days)
}

# Function to find daily IRR using numerical method
find_daily_irr <- function(cash_flows, initial_guess = 0.0001) {
  uniroot(
    f = function(r) calculate_npv(r, cash_flows),
    interval = c(-0.1, 0.1),
    tol = 1e-10
  )$root
}

# Calculate daily IRR
daily_irr <- find_daily_irr(cash_flows)

# Annualize the daily IRR (365-day compounding)
annual_irr <- (1 + daily_irr)^365 - 1

# Create results dataframe
results <- data.frame(
  Daily_IRR = daily_irr,
  Annual_IRR = annual_irr
)

# Save results
write.csv(results, "q8_irr_results.csv", row.names = FALSE)

# Print results
print("Daily IRR:")
print(daily_irr)
print("Annualized IRR (365-day compounding):")
print(annual_irr)