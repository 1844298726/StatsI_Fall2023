### Question 1: Political Science
# a
# Define the observed frequency matrix
observed_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Perform a chi-squared test
test_result <- chisq.test(observed_data)

# Calculate row sums for the contingency table
row_sums <- rowSums(observed_data)

# Calculate column sums for the table
col_sums <- colSums(observed_data)

# Compute the overall total count
total_count <- sum(observed_data)

# Calculate the expected frequency table
expected_data <- outer(row_sums, col_sums) / total_count

# Calculate the chi-squared values
chi_squared_values <- (observed_data - expected_data)^2 / expected_data

# Calculate the chi-squared test statistic
chi_squared_statistic <- sum(chi_squared_values)
cat("Chi-Squared Test Statistic: ", chi_squared_statistic, "\n")


# b 
# Degrees of freedom
# Calculate the degrees of freedom for the chi-squared test
df <- (nrow(observed_data) - 1) * (ncol(observed_data) - 1)

# Calculate the p-value for the chi-squared test
p_value <- 1 - pchisq(chi_squared_statistic, df)

# Set the significance level (alpha)
alpha <- 0.1

# Print the calculated p-value
cat("P-Value: ", p_value, "\n")

# Perform a hypothesis test based on the p-value and significance level
if (p_value <= alpha) {
  cat("Reject the null hypothesis. There is evidence of an association between variables.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no strong evidence of an association between variables.\n")
}


# c
# Calculate the standardized residuals for each cell
standardized_residuals <- (observed_data - expected_data) / sqrt(expected_data)

# Create an empty result table
result_table <- matrix(NA, nrow = 2, ncol = 3)
colnames(result_table) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(result_table) <- c("Upper class", "Lower class")

# Format the standardized residuals with two decimal places
result_table <- format(standardized_residuals, digits = 2)

# Set row names for the result table
rownames(result_table) <- c("Upper class", "Lower class")

print(result_table)


######  Question 2: Economics
## Load the data
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
#Run the bivariate regression
model <- lm(water ~ reserved, data = data)
## Summarize the regression results
summary(model)
