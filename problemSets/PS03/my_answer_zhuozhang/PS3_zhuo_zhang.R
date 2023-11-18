#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load necessary libraries
library(ggplot2)
library(tidyr)
# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
#Question1:
# Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.
model1 <- lm(voteshare ~ difflog, data = inc.sub)

# Summary of the regression model
summary(model1)

# Create a scatterplot of difflog and voteshare
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of difflog vs. voteshare",
       x = "Difference in Campaign Spending (log scale)",
       y = "Vote Share")

# Save residuals of the model
residuals <- resid(model1)
write.csv(data.frame(residuals), "residuals.csv")

# Getting the coefficients of the regression model
coefficients <- coef(model1)

# Extract the intercept and slope
intercept <- coefficients[1]
slope <- coefficients[2]

# Output prediction equation
cat("prediction equation: voteshare =", round(intercept, 2), "+", round(slope, 2), "* difflog\n")

#If the lm function is not used, it can be explained in detail with the following procedure.
# Extraction of relevant variables
x <- inc.sub$difflog
y <- inc.sub$voteshare

# Calculate the mean value
x_mean <- mean(x)
y_mean <- mean(y)

# Calculation of regression coefficients
beta_1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)
beta_0 <- y_mean - beta_1 * x_mean

# Print Factor
cat("ratio (beta_1):", beta_1, "\n")
cat("intercept (beta_0):", beta_0, "\n")
cat("prediction equation: voteshare =", round(beta_0, 2), "+", round(beta_1, 2), "* difflog\n")
#Question2:

# Run a regression where the outcome variable is presvote and the explanatory variable is difflog
model2  <- lm(presvote ~ difflog, data = inc.sub)  

# Summary of the regression model
summary(model2)

# Create a scatterplot with a regression line
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of difflog vs. presvote",
       x = "Difference in Incumbent and Challenger Spending (log scale)",
       y = "Presidential Candidate Vote Share")

# Save residuals of the model
residuals <- resid(model2)

# Extract the intercept and slope
coefficients <- coef(model2)
intercept <- coefficients[1]
slope <- coefficients[2]

# Output prediction equation
cat("prediction equation: presvote =", round(intercept, 2), "+", round(slope, 2), "* difflog\n")

#Question3:

# Run a regression where the outcome variable is voteshare and the explanatory variable is presvote
model3 <- lm(voteshare ~ presvote, data = inc.sub)

# Summary of the regression model
summary(model3)

# Make a scatterplot of the two variables and add the regression line
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "presvote vs. voteshare Scatterplot",
       x = "Presidential Candidate's Vote Share",
       y = "Vote Share of Incumbent's Party Candidate")

# Extract coefficients
coefficients <- coef(model3)
intercept <- coefficients[1]
slope <- coefficients[2]

# Write prediction equation
cat("prediction equation: voteshare =", round(intercept, 2), "+", round(slope, 2), "* presvote\n")

#Question4:

# Extract residuals from the first regression
residuals1 <- resid(model1)

# Extract residuals from the second regression
residuals2 <- resid(model2)

# Run a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question
model4 <- lm(residuals2 ~ residuals1)

# Summary of the regression model
summary(model4)

# Scatterplot with regression line for residuals
ggplot(data.frame(residuals1, residuals2), aes(x = residuals1, y = residuals2)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals Scatterplot",
       x = "Residuals from Question 1",
       y = "Residuals from Question 2")

# Extract coefficients for the regression with residuals
coefficients_residuals <- coef(model4)
intercept_residuals <- coefficients_residuals[1]
slope_residuals <- coefficients_residuals[2]

# Write prediction equation for residuals
cat("Prediction equation for residuals: residuals2 =", round(intercept_residuals, 2), "+", round(slope_residuals, 2), "* residuals1\n")

#Question5:

# Run a regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote.
model_multiple <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# Summary of the regression model
summary(model_multiple)

# Extract coefficients for the multiple regression
coefficients_multiple <- coef(model_multiple)
intercept_multiple <- coefficients_multiple[1]
slope_difflog <- coefficients_multiple[2]
slope_presvote <- coefficients_multiple[3]

# Write prediction equation for multiple regression
cat("Prediction equation for multiple regression: voteshare =", round(intercept_multiple, 2), "+", round(slope_difflog, 2), "* difflog +", round(slope_presvote, 2), "* presvote\n")
