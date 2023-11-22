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

install.packages("Rtools")
library("Rtools")

install.packages("Datacar")
library("Datacar")

install.packages("car")
library("car")
data(Prestige)
help(Prestige)
# Inspect and summarize the data.
head(Prestige) # First 6 rows of dataset
str(Prestige) # Structure of Prestige dataset
summary(Prestige) # Summarize the data of Prestige dataset
#Question 1
#(a)
# Create a new variable 'professional'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
#(b)
# Run the linear model
model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)  # View the model summary
pdf("Q1_b.pdf")
# Draw graphics: preview vs. income, and differentiate with different colors based on occupation
plot(Prestige$income, Prestige$prestige,
     col = ifelse(Prestige$professional == 1, "blue", "red"),
     pch = 16,  # Set Scatter Chart Style
     ylab = "prestige", xlab = "income")

# Add regression line
abline(model, col = "green", lty = 2)
legend("topright", legend = c("Non-Professional", "Professional", "Regression Line"),
       col = c("red", "blue", "green"), pch = c(16, 16, NA))
dev.off()
#(c)
# Extract model coefficients
coefficients <- coef(model)

# Identify the various coefficients in the prediction equation
intercept <- coefficients["(Intercept)"]
income_coef <- coefficients["income"]
professional_coef <- coefficients["professional"]
income_professional_coef <- coefficients["income:professional"]

# Print prediction equation
cat("prediction equation：prestige =", round(intercept, 2), "+", round(income_coef, 2), "* income +",
    round(professional_coef, 2), "* professional +", round(income_professional_coef, 2), "* income * professional")
