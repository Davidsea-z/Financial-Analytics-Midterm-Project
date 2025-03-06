# Load necessary libraries
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)

# Load data, skipping first three lines
data <- read.csv("Nikkei225.csv", skip = 3)

# Select required columns and convert date
data <- data %>%
  select(Date, Close, Open, High, Low) %>%
  mutate(Date = parse_date_time(Date, orders = "dmY"))

# Calculate Heikin Ashi closing price
data <- data %>%
  mutate(HClose = (Close + Open + High + Low) / 4)

# Calculate moving averages for both Close and HClose
data <- data %>%
  arrange(Date) %>%
  mutate(
    MA5_Close = rollmean(Close, k = 5, fill = NA, align = "right"),
    MA150_Close = rollmean(Close, k = 150, fill = NA, align = "right"),
    MA5_HClose = rollmean(HClose, k = 5, fill = NA, align = "right"),
    MA150_HClose = rollmean(HClose, k = 150, fill = NA, align = "right")
  )

# Generate signals
data <- data %>%
  mutate(
    signal_Close = ifelse(MA5_Close > MA150_Close, 1, 0),
    signal_HClose = ifelse(MA5_HClose > MA150_HClose, 1, 0)
  )

# Calculate forward daily returns using log return
data <- data %>%
  mutate(
    forward_return = c(diff(log(Close)), NA)
  )

# Calculate strategy returns
data <- data %>%
  mutate(
    return_Close = lag(signal_Close) * forward_return,
    return_HClose = lag(signal_HClose) * forward_return
  )

# Make data tidy and filter NA values
long_data <- data %>%
  select(Date, return_Close, return_HClose) %>%
  pivot_longer(
    cols = c(return_Close, return_HClose),
    names_to = "Strategy",
    values_to = "Return"
  ) %>%
  filter(!is.na(Return))

# Add Year and Month as factors
long_data <- long_data %>%
  mutate(
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date))
  )

# Run ANOVA with blocking factors
anova_model <- aov(Return ~ Strategy + Year + Month, data = long_data)

# Save results
write.csv(summary(anova_model)[[1]], "q7_anova_results.csv", row.names = TRUE)

# Save summary statistics
summary_stats <- long_data %>%
  group_by(Strategy) %>%
  summarise(
    Mean = mean(Return, na.rm = TRUE),
    SD = sd(Return, na.rm = TRUE),
    N = n()
  )
write.csv(summary_stats, "q7_summary_stats.csv", row.names = FALSE)

# Save strategy signals
signals_summary <- data %>%
  summarise(
    Close_Buy_Signals = sum(signal_Close, na.rm = TRUE),
    HClose_Buy_Signals = sum(signal_HClose, na.rm = TRUE)
  )
write.csv(signals_summary, "q7_signals_summary.csv", row.names = FALSE)