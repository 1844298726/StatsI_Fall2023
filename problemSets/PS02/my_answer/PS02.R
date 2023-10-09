### Question 1: Political Science
# a
# Create the observed frequency table
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
chisq.test(observed)

# Calculate row totals
row_totals <- rowSums(observed)

# Calculate column totals
col_totals <- colSums(observed)

# Calculate the overall total
total <- sum(observed)

# Calculate the expected frequency table
expected <- outer(row_totals, col_totals) / total

# Calculate chi-squared values
chi_square_values <- (observed - expected)^2 / expected

# Calculate the chi-squared test statistic
chi_square_statistic <- sum(chi_square_values)
cat("Chi-Squared Test Statistic: ", chi_square_statistic, "\n")




# b 
# Degrees of freedom
df <- (nrow(observed) - 1) * (ncol(observed) - 1)

p_value <- 1 - pchisq(chi_square_statistic, df)

alpha <- 0.1
cat("P-Value: ", p_value, "\n")
if (p_value <= alpha) {
  cat("Reject the null hypothesis. There is evidence of an association between variables.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no strong evidence of an association between variables.\n")
}

# c
# Calculate the standardized residuals for each cell
standardized_residuals <- (observed - expected) / sqrt(expected)

result_table <- matrix(NA, nrow = 2, ncol = 3)
colnames(result_table) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(result_table) <- c("Upper class", "Lower class")
result_table <- format(standardized_residuals, digits = 2)
rownames(result_table) <- c("Upper class", "Lower class")

result_table

# d
# For the cell corresponding to "Upper class" and "Not Stopped," the standardized residual is 0.14. Since it is close to zero, it suggests that the observed and expected frequencies for this cell are reasonably close, and there may not be a strong association between being in the upper class and not getting stopped.

# For the cell corresponding to "Upper class" and "Bribe requested," the standardized residual is -0.82. This negative value indicates that the observed frequency of upper-class individuals requesting a bribe is lower than expected. It suggests that being in the upper class might be associated with a lower likelihood of requesting a bribe.

# For the cell corresponding to "Upper class" and "Stopped/given warning," the standardized residual is 0.82. This positive value suggests that the observed frequency of upper-class individuals being stopped or given a warning is higher than expected. It implies that being in the upper class might be associated with a higher likelihood of being stopped or given a warning.

# For the cell corresponding to "Lower class" and "Not Stopped," the standardized residual is -0.18, which is close to zero. It suggests that there may not be a strong association between being in the lower class and not getting stopped.

# For the cell corresponding to "Lower class" and "Bribe requested," the standardized residual is 1.09, indicating that the observed frequency of lower-class individuals requesting a bribe is higher than expected. It suggests that being in the lower class might be associated with a higher likelihood of requesting a bribe.

# For the cell corresponding to "Lower class" and "Stopped/given warning," the standardized residual is -1.10, indicating that the observed frequency of lower-class individuals being stopped or given a warning is lower than expected. It implies that being in the lower class might be associated with a lower likelihood of being stopped or given a warning.



######  Question 2: Economics
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

model <- lm(water ~ reserved, data = data)
summary(model)
