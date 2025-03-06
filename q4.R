# Load necessary libraries
library(dplyr)
library(tidyr)
library(broom)

# Load data
factors <- read.csv("factor.csv")
returns <- read.csv("ret.csv")

# Convert Date columns to proper format
factors$Date <- as.Date(factors$date, format = "%m/%d/%Y")
returns$Date <- as.Date(returns$date, format = "%m/%d/%Y")

# Part a: Regress excess return on factors for IBM
# Filter IBM returns and combine with factors
regression_data <- returns %>%
  filter(PERMNO == 12490) %>%  # Filter for IBM
  mutate(IBM_excess = RET - factors$rf[match(Date, factors$Date)]) %>%  # Calculate excess return
  left_join(factors, by = "Date")

# Perform regression for IBM
ibm_model <- lm(IBM_excess ~ mktrf + smb + hml, data = regression_data)

# Get summary statistics
ibm_summary <- summary(ibm_model)

# Test hml coefficient
hml_test <- coef(summary(ibm_model))["hml", ]

# Save IBM regression results
write.csv(tidy(ibm_model), "q4_ibm_regression.csv", row.names = FALSE)
write.csv(data.frame(
  coefficient = hml_test["Estimate"],
  std_error = hml_test["Std. Error"],
  t_value = hml_test["t value"],
  p_value = hml_test["Pr(>|t|)"]
), "q4_hml_test.csv", row.names = FALSE)

# Part b: Analyze cma factor
# Calculate excess return for cma
factors$cma_excess <- factors$cma - factors$rf

# Regress cma excess return on factors
cma_model <- lm(cma_excess ~ mktrf + smb + hml, data = factors)

# Get summary statistics
cma_summary <- summary(cma_model)

# Save CMA regression results
write.csv(tidy(cma_model), "q4_cma_regression.csv", row.names = FALSE)

# Save intercept test results (alpha) for abnormal returns analysis
alpha_test <- coef(summary(cma_model))["(Intercept)", ]
write.csv(data.frame(
  alpha = alpha_test["Estimate"],
  std_error = alpha_test["Std. Error"],
  t_value = alpha_test["t value"],
  p_value = alpha_test["Pr(>|t|)"]
), "q4_cma_alpha_test.csv", row.names = FALSE)