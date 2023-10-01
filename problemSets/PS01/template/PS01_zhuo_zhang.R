
# 1
# Given data
IQ_scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Perform a t-test
t_test_result <- t.test(IQ_scores, conf.level = 0.90)
lower <- t_test_result$conf.int[1]
upper <- t_test_result$conf.int[2]
# Print t-test results
cat("1. 90% Confidence Interval for School Students' Average IQ:\n")
cat( lower, "-", upper, "\n")


#Perform a t-test
t_test_result <- t.test(IQ_scores, mu = 100, alternative = "greater")

# Print t-test results
cat("2.Hypothesis Test Results:\n")
cat("Test Statistic (t):", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

# Determine whether to reject the null hypothesis based on the p-value
if (t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis: the average IQ of students in the school is above 100.\n")
} else {
  cat("Null hypothesis not rejected: There is insufficient evidence that the average IQ of students in the school is above 100.\n")
}
# 2


df <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

# Scatterplot matrix
pairs(df[c("Y", "X1", "X2", "X3")], main="Scatterplot Matrix")

# Calculate correlations
correlations <- cor(df[c("Y", "X1", "X2", "X3")])
print(correlations)

install.packages('ggplot2')
# Create a boxplot
library(ggplot2)
ggplot(df, aes(x = Region, y = Y)) +
  geom_boxplot() +
  labs(x = "Region", y = "Per Capita Expenditure") +
  theme_minimal()
# Create a scatterplot 
library(ggplot2)
ggplot(df, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point() +
  labs(x = "Per Capita Income", y = "Per Capita Expenditure") +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18, "4" = 19)) +
  theme_minimal()