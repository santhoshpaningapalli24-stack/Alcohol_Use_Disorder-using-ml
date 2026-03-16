# Generating Synthetic Dataset for AUD Risk Identification
# This creates a realistic dataset with multiple risk factors

set.seed(123)  # For reproducibility

# Number of observations
n <- 1000

# Generate synthetic data
aud_data <- data.frame(
  # Demographics
  age = round(rnorm(n, mean = 35, sd = 12)),
  gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)),
  
  # Family history (strong predictor)
  family_history = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
  
  # Mental health factors
  depression_score = round(rnorm(n, mean = 15, sd = 8)),  # 0-40 scale
  anxiety_score = round(rnorm(n, mean = 12, sd = 7)),     # 0-35 scale
  
  # Social factors
  peer_drinking = sample(c("Low", "Medium", "High"), n, replace = TRUE, 
                         prob = c(0.3, 0.4, 0.3)),
  marital_status = sample(c("Single", "Married", "Divorced"), n, 
                          replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  
  # Behavioral factors
  age_first_drink = round(rnorm(n, mean = 18, sd = 4)),
  drinks_per_week = round(rgamma(n, shape = 2, rate = 0.5)),
  
  # Stress and lifestyle
  stress_level = round(rnorm(n, mean = 6, sd = 2.5)),    # 0-10 scale
  employment_status = sample(c("Employed", "Unemployed", "Student"), n, 
                             replace = TRUE, prob = c(0.65, 0.15, 0.2))
)

# Constrain values to realistic ranges
aud_data$age[aud_data$age < 18] <- 18
aud_data$age[aud_data$age > 80] <- 80
aud_data$depression_score[aud_data$depression_score < 0] <- 0
aud_data$depression_score[aud_data$depression_score > 40] <- 40
aud_data$anxiety_score[aud_data$anxiety_score < 0] <- 0
aud_data$anxiety_score[aud_data$anxiety_score > 35] <- 35
aud_data$stress_level[aud_data$stress_level < 0] <- 0
aud_data$stress_level[aud_data$stress_level > 10] <- 10
aud_data$age_first_drink[aud_data$age_first_drink < 10] <- 10
aud_data$age_first_drink[aud_data$age_first_drink > 30] <- 30

# Generate outcome variable (AUD Risk) based on risk factors
# This creates a realistic relationship between predictors and outcome
aud_data$risk_score <- with(aud_data,
                            0.3 * (family_history == "Yes") +
                              0.2 * (gender == "Male") +
                              0.15 * (depression_score / 40) +
                              0.15 * (anxiety_score / 35) +
                              0.1 * (peer_drinking == "High") +
                              0.05 * (marital_status == "Divorced") +
                              0.1 * (age_first_drink < 16) +
                              0.15 * (drinks_per_week / max(drinks_per_week)) +
                              0.1 * (stress_level / 10) +
                              0.05 * (employment_status == "Unemployed") +
                              rnorm(n, 0, 0.15)  # Add some randomness
)

# Convert risk score to binary outcome (High Risk vs Low Risk)
aud_data$aud_risk <- ifelse(aud_data$risk_score > median(aud_data$risk_score), 
                            "High Risk", "Low Risk")
aud_data$aud_risk <- factor(aud_data$aud_risk, levels = c("Low Risk", "High Risk"))

# Remove the intermediate risk_score column
aud_data$risk_score <- NULL

# Display first few rows
head(aud_data)

# Summary statistics
summary(aud_data)

# Check class distribution
table(aud_data$aud_risk)

# Save dataset to CSV
write.csv(aud_data, "aud_risk_dataset.csv", row.names = FALSE)

cat("\nDataset created successfully!")
cat("\nTotal observations:", nrow(aud_data))
cat("\nHigh Risk cases:", sum(aud_data$aud_risk == "High Risk"))
cat("\nLow Risk cases:", sum(aud_data$aud_risk == "Low Risk"))

