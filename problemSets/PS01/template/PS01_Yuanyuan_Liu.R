#Question1.1

IQ_scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t_test_result <- t.test(IQ_scores,conf.level = 0.90)

min <- t_test_result$conf.int[1]
max <- t_test_result$conf.int[2]
cat("1. 90% Confidence Interval for School Students' Average IQ:\n")
cat(min,max)

#Question1.2
t_test_result <- t.test(IQ_scores,mu=100,alternative="greater")
t_test_result
cat("\nHypothes Test Results:\n")
cat("Test Statistic (t):", t_test_result$statistic, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")
cat("p-value:", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  cat("we reject the null hypothesis. School students' average IQ is higher than the national average.\n")
} else {
  cat("we fail to reject the null hypothesis. School students' average IQ is equal to the national average.\n")
}

install.packages('ggplot2')
# Create a boxplot
library(ggplot2)

#Question2.1
data_expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

pairs(data_expenditure[c("Y", "X1", "X2", "X3")], main="Scatterplot Matrix")

correlations <- cor(data_expenditure[c("Y", "X1", "X2", "X3")])
print(correlations)

#Question2.2
ggplot(data_expenditure, aes(x = factor(Region), y = Y)) +
  geom_boxplot() +
  labs(x = "Region", y = "Per Capita Expenditure") +
  ggtitle("relationship:Region VS Y")

#Question2.3
ggplot(data_expenditure, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point(size=3) +
  labs(x = "Per Capita Income", y = "Per Capita Expenditure") +
  ggtitle("relationship:X1 VS Y")
scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18, "4" = 19)) +
  theme_minimal()
