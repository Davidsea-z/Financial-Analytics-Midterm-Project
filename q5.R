# Load necessary libraries
library(dplyr)

# Read the data
data <- read.csv("return2.csv")

# Separate benchmark and portfolio data
benchmark <- data %>%
  filter(Portfolio == "Benchmark") %>%
  select(Sector, Weight, Return)
colnames(benchmark) <- c("Sector", "BWeight", "BReturn")

portfolio <- data %>%
  filter(Portfolio == "CUHKFund") %>%
  select(Sector, Weight, Return)
colnames(portfolio) <- c("Sector", "PWeight", "PReturn")

# Merge benchmark and portfolio data
attribution <- merge(benchmark, portfolio, by = "Sector")

# Calculate effects
attribution <- attribution %>%
  mutate(
    # Convert percentages to decimals
    BWeight = BWeight/100,
    PWeight = PWeight/100,
    BReturn = BReturn/100,
    PReturn = PReturn/100,
    
    # Calculate effects
    Allocation = (PWeight - BWeight) * (BReturn - mean(BReturn)),
    Selection = BWeight * (PReturn - BReturn),
    Interaction = (PWeight - BWeight) * (PReturn - BReturn)
  )

# Calculate total effects
total_effects <- data.frame(
  Effect = c("Allocation", "Selection", "Interaction", "Total"),
  Value = c(
    sum(attribution$Allocation),
    sum(attribution$Selection),
    sum(attribution$Interaction),
    sum(attribution$Allocation) + sum(attribution$Selection) + sum(attribution$Interaction)
  )
)

# Convert back to percentage for output
attribution[, c("BWeight", "PWeight", "BReturn", "PReturn", "Allocation", "Selection", "Interaction")] <-
  attribution[, c("BWeight", "PWeight", "BReturn", "PReturn", "Allocation", "Selection", "Interaction")] * 100

total_effects$Value <- total_effects$Value * 100

# Save detailed attribution results
write.csv(attribution, "q5_attribution_details.csv", row.names = FALSE)

# Save total effects
write.csv(total_effects, "q5_total_effects.csv", row.names = FALSE)

# Calculate and save brief description
description <- paste(
  "BHB Return Attribution Analysis Results:\n",
  "1. Total attribution effect:", round(total_effects$Value[4], 2), "%\n",
  "2. Main components:\n",
  "   - Allocation effect:", round(total_effects$Value[1], 2), "%\n",
  "   - Selection effect:", round(total_effects$Value[2], 2), "%\n",
  "   - Interaction effect:", round(total_effects$Value[3], 2), "%\n",
  sep = ""
)

writeLines(description, "q5_description.txt")