# ----- B1705 Week 3 | Canonical Correlation Analysis (CCA) | 31.01.2024 -----

# ----- PRE-LECTURE WORK -----
# ----- 1. Exploring the Data -----
# Load necessary library
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 600

# Means and standard deviations for all variables including 'motivation' as continuous
means <- c(locus_of_control = 0.0965333, self_concept = 0.0049167, 
           motivation = 0.6608333, speed = 51.90183, strength = 52.38483, 
           agility = 51.849, endurance = 51.76333)
sds <- c(locus_of_control = 0.6702799, self_concept = 0.7055125, 
         motivation = 0.3427294, speed = 10.10298, strength = 9.726455, 
         agility = 9.414736, endurance = 9.706179)

# Define a covariance matrix with some arbitrary correlations
cor_matrix <- matrix(c(
  1.00, 0.50, 0.30, 0.30, 0.60, 0.20, 0.60, # locus_of_control correlations
  0.50, 1.00, 0.25, 0.25, 0.55, 0.15, 0.75, # self_concept correlations
  0.30, 0.25, 1.00, 0.40, 0.60, 0.30, 0.50, # motivation correlations
  0.30, 0.25, 0.40, 1.00, 0.60, 0.40, 0.40, # speed correlations
  0.20, 0.15, 0.40, 0.60, 1.00, 0.45, 0.45, # strength correlations
  0.20, 0.15, 0.30, 0.40, 0.60, 1.00, 0.50, # agility correlations
  0.20, 0.15, 0.30, 0.40, 0.7, 0.50, 1.00   # endurance correlations
), 7, 7)

# Convert correlation matrix to covariance matrix
cov_matrix <- diag(sds) %*% cor_matrix %*% diag(sds)

# Generate multivariate normal data
set.seed(123) # for reproducibility
data <- mvrnorm(n, mu = means, Sigma = cov_matrix)

# Convert to data frame and name columns
data <- as.data.frame(data)
names(data) <- c("locus_of_control", "self_concept", "motivation", "speed", "strength", "agility", "endurance")

rm(cor_matrix, cov_matrix)

# ----- 2. Examining Correlation Matrix between variables -----
# Load the corrplot package
library(corrplot)

cor_matrix <- cor(data) # create the correlation matrix
cov_matrix <- cov(data) # create the covariance matrix

# Visualise  correlation matrix
corrplot(cor_matrix, method = "number")

# ----- 3. Conducting the CCA Analysis -----
# Loading packages, defining variables 
library(ggplot2)
library(GGally)
library(CCA)
library(CCP)

psych <- data[,1:3]
sport <- data[,4:7]

# Looking at correlations within and between two sets of variables using matcor function
matcor(psych, sport)

# Running the CCA Analysis on our two variable sets
cc1 <- cc(psych, sport) 

# Displaying the canonical correlations
cc1$cor

# Examining the raw Canonical Coefficients 
# print the raw canonical coefficients
cc1[3:4]

# Computing the loadings of the variables
# compute canonical loadings
cc2 <- comput(psych, sport, cc1)

# display canonical loadings
cc2[3:6]

# Statistical Testing using 'CCP'
# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(psych)[1]
p <- length(psych)
q <- length(sport)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

# ----- 4. Computing the standardised canonical coefficients -----

# standardised psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardised acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(sport))))
s2 %*% cc1$ycoef

# ----- LECTURE WORK -----

# Canonical Correlation - Practical

# ----- 6. Create Synthetic Dataset -----

# clean environment
rm(list = ls())

# Load necessary library
library(MASS)  # For generating correlated data

# Create a synthetic dataset
create_dataset <- function(n = 100, means, Sigma) {
  data <- MASS::mvrnorm(n = n, mu = means, Sigma = Sigma)
  colnames(data) <- c("DrivingAccuracy", "PuttingAccuracy", "AverageScore", "EaglesPerRound", 
                      "WindSpeed", "Rainfall", "Temperature", "CourseDifficulty")
  return(as.data.frame(data))
}

# Define means and a covariance matrix for the variables
means <- c(60, 50, 72, 0.5, 10, 5, 70, 7)
Sigma <- matrix(c(
  1, 0.2, 0, 0, -0.6, -0.7, 0, -0.1,
  0.2, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, -0.5,
  0, 0, 0, 1, 0, 0, 0, 0,
  -0.6, 0, 0, 0, 1, 0, 0, 0,
  -0.7, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0,
  -0.1, 0, -0.5, 0, 0, 0, 0, 1
), ncol = 8)

# Generate the dataset
set.seed(123) # For reproducibility
golf_dataset <- create_dataset(n = 100, means, Sigma)


# Round all columns in the dataframe to 2 decimal places
golf_dataset <- data.frame(lapply(golf_dataset, function(x) {
  if(is.numeric(x)) round(x, 2) else x
}))

rm(Sigma)

# ----- 7. Examining Correlations -----

# Load packages
library(ggplot2)
library(corrplot)

cor_matrix <- cor(golf_dataset) # create the correlation matrix
cov_matrix <- cov(golf_dataset) # create the covariance matrix

# Visualise  correlation matrix
corrplot(cor_matrix, method = "number")

# more visualisations of the variables
# Example 1

# Scatterplot for Putting Accuracy vs Rainfall
ggplot(golf_dataset, aes(x = Rainfall, y = PuttingAccuracy)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatterplot of Putting Accuracy vs Rainfall",
       x = "Rainfall (mm)",
       y = "Putting Accuracy (%)") +
  theme_minimal()


## Example 2
# Categorise Wind Speed into 'Low', 'Medium', and 'High'
golf_dataset$WindSpeedCategory <- cut(golf_dataset$WindSpeed, 
                                      breaks = quantile(golf_dataset$WindSpeed, probs = 0:3/3),
                                      labels = c("Low", "Medium", "High"), 
                                      include.lowest = TRUE)

# Nested Scatterplot
ggplot(golf_dataset, aes(x = DrivingAccuracy, y = AverageScore)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ WindSpeedCategory, scales = "free") +
  labs(title = "Nested Scatterplot of Driving and Average Score by Wind Speed",
       x = "Driving Accuracy (%)",
       y = "Average Score (%)",
       caption = "Wind Speed Categories: Low, Medium, High") +
  theme_minimal()

# ----- 8. Load Libraries for CCA ----- 

library(ggplot2)
library(GGally)
library(CCA)
library(CCP)

# ----- 9. Create Variable Sets ----

performance <- golf_dataset[,1:4]
conditions <- golf_dataset[,5:8]

# ----- 10. Calculate Canonical Correlation -----

matcor(performance, conditions)

# ----- 11. Display Canonical Correlations -----

cc1 <- cc(performance, conditions) 

# display the canonical correlations
cc1$cor

# ----- 12. Display Raw Canonical Correlations -----

# print the raw canonical coefficients
cc1[3:4]

# ----- 13. Display Canonical Loading -----

# compute canonical loadings
cc2 <- comput(performance, conditions, cc1)

# display canonical loadings
cc2[3:6]

# ----- 14. Test the Canonical Dimensions -----

# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(conditions)[1]
p <- length(conditions)
q <- length(performance)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

# standardised conditions canonical coefficients diagonal matrix of conditions sd's

s1 <- diag(sqrt(diag(cov(conditions))))
s1 %*% cc1$xcoef

# standardised performance canonical coefficients diagonal matrix of performance sd's

s2 <- diag(sqrt(diag(cov(performance))))
s2 %*% cc1$ycoef

# ----- 15. Canonical Correlation: Practical -----


rm(list = ls())

# Load necessary library
library(MASS)  # For generating correlated data

# Function to create a synthetic dataset
create_dataset <- function(n = 100, means, Sigma) {
  data <- MASS::mvrnorm(n = n, mu = means, Sigma = Sigma)
  colnames(data) <- c("CalorieIntake", "ProteinContent", "VitaminLevels", "WaterIntake",
                      "StressLevel", "MoodRating", "SleepQuality", "ConcentrationLevel")
  return(as.data.frame(data))
}

# Define means and a covariance matrix for the variables
# These are example values and can be adjusted
means <- c(2000, 50, 100, 2, 5, 7, 7, 7)  # Example mean values for each variable
Sigma <- matrix(c(
  1, 0.5, 0, 0, 0, 0.3, 0.3, 0,
  0.5, 1, 0, 0, 0, 0.2, 0.4, 0,
  0, 0, 1, 0, -0.2, 0.2, 0, 0,
  0, 0, 0, 1, -0.3, 0.1, 0.2, 0.1,
  0, 0, -0.2, -0.3, 1, -0.4, -0.5, -0.3,
  0.3, 0.2, 0.2, 0.1, -0.4, 1, 0.6, 0.4,
  0.3, 0.4, 0, 0.2, -0.5, 0.6, 1, 0.5,
  0, 0, 0, 0.1, -0.3, 0.4, 0.5, 1
), ncol = 8)

# Generate the dataset
set.seed(123) # For reproducibility
health_dataset <- create_dataset(n = 500, means, Sigma)

# Calculate  correlation and covariance matrix

cor_matrix <- cor(health_dataset) # create the correlation matrix
cov_matrix <- cov(health_dataset) # create the covariance matrix

##### 15.1. Loading the dataset and visualising correlations #####
# Load packages
library(ggplot2)
library(corrplot)

cor_matrix <- cor(health_dataset) # create the correlation matrix
cov_matrix <- cov(health_dataset) # create the covariance matrix

# Visualise  correlation matrix
corrplot(cor_matrix, method = "number")


##### 15.2. Visualisations and Examples #####
# More visualisations of the variables. Example 1 and 2 use select variables, the correlation matrix shows all variables as a value in relation
# Example 1

# Scatterplot of Calorie Intake and Sleep Quality
ggplot(health_dataset, aes(x = CalorieIntake, y = SleepQuality)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatterplot of Calorie Intake and Sleep Quality",
       x = "Intake (cals)",
       y = "Sleep Quality (low - high)") +
  theme_bw()

## Example 2
# Categorise Mood Rating into 'Low', 'Medium', and 'High'
health_dataset$MoodRatingCat <- cut(health_dataset$MoodRating, 
                                    breaks = quantile(health_dataset$MoodRating, probs = 0:3/3),
                                    labels = c("Low", "Medium", "High"), 
                                    include.lowest = TRUE)

# Nested Scatterplot
ggplot(health_dataset, aes(x = WaterIntake, y = ConcentrationLevel)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ MoodRatingCat, scales = "free") +
  labs(title = "Nested Scatterplot of Water Intake and Concentration Level by Mood Rating",
       x = "Water Intake (l)",
       y = "Concentration Level (low - high)",
       caption = "Mood Categories: Low, Medium, High") +
  theme_minimal()

##### 15.3. Loading libraries and partitioning dataset into nutritional and health variables #####
library(ggplot2)
library(GGally)
library(CCA)
library(CCP)


nutrition <- health_dataset[,1:4]
health <- health_dataset[,5:8]

matcor(health, nutrition)


##### 15.4. Displaying canonical correlations #####
cc1 <- cc(health, nutrition) 

# display the canonical correlations
cc1$cor

##### 15.5. Printing raw canonical coefficients #####
cc1[3:4]

##### 15.6 Computing canonical loadings #####
cc2 <- comput(health, nutrition, cc1)

# display canonical loadings
cc2[3:6]

# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(nutrition)[1]
p <- length(nutrition)
q <- length(health)

##### 15.7. Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

##### 15.8. Standardising ##### 
# nutrition canonical coefficients diagonal matrix of nutrition sd's
s1 <- diag(sqrt(diag(cov(nutrition))))
s1 %*% cc1$xcoef

# tandardised health canonical coefficients diagonal matrix of health sd's
s2 <- diag(sqrt(diag(cov(health))))
s2 %*% cc1$ycoef
