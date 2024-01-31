# ----- B1705 Week 3 | Discriminant Analysis | 31.01.2024 -----

# ----- PRE-LECTURE WORK -----
# ----- 1. Setting up environment -----
# Load necessary libraries for DA
library(MASS)
library(ggplot2)
library(caret)
library(ggfortify)

# Load the iris dataset
data(iris)
set.seed(123)  # Setting seed for reproducibility

# Splitting the dataset into training and testing sets
index <- createDataPartition(iris$Species, p=0.7, list=FALSE)
train_data <- iris[index,]
test_data <- iris[-index,]

# ----- 2. LDA Model Training -----

# Fitting LDA model
lda_model <- lda(Species ~ ., data=train_data)

# Summary of the model
print(lda_model)

# ----- 3. LDA Model Evaluation -----

# Predicting on test data
lda_pred <- predict(lda_model, test_data)
confusionMatrix(lda_pred$class, test_data$Species)

# ----- 4. Example LDA Plots of Discriminant Analysis -----

##### 4.1. Plot 1 #####
# Load libraries
library(MASS)  # For LDA

# Load the iris dataset
data(iris)

# Perform LDA
lda_model <- lda(Species ~ ., data = iris)

# Predict using the LDA model
lda_pred <- predict(lda_model)

# Add LDA components to the original data
iris_lda <- cbind(iris, lda_pred$x)

# Plot 1: LDA Components
ggplot(iris_lda, aes(LD1, LD2, color = Species)) +
  geom_point() +
  ggtitle("LDA: Iris Data") +
  xlab("Linear Discriminant 1") +
  ylab("Linear Discriminant 2")

##### 4.2. Plot 2 (Includes Confidence Ellipses) #####

# Plot 2: LDA Components with Ellipses
ggplot(iris_lda, aes(LD1, LD2, color = Species)) +
  geom_point() +
  stat_ellipse(type = "norm") +
  ggtitle("LDA with Confidence Ellipses") +
  xlab("Linear Discriminant 1") +
  ylab("Linear Discriminant 2")

##### 4.3. Plot 3 Boxplot #####

# Plot 3: LDA Component 1 by Species
ggplot(iris_lda, aes(Species, LD1, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of LD1 by Species")

##### 4.4. Plot 4 Boxplot 2 ##### 

# Plot 5: LDA Component 2 by Species
ggplot(iris_lda, aes(Species, LD2, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of LD2 by Species")


# ----- LECTURE WORK -----

# ----- 5. Loading libraries -----

# install packages
library(ggfortify)
library(ggplot2)
library(caret)
library(MASS)

# Creating object called lda_data
lda_data <- read.csv('https://www.dropbox.com/scl/fi/tnbw8s1vbndbfu3zalw4c/lda_01.csv?rlkey=cahnq1v5e197pdv0al1dll99g&dl=1')

lda_data$FavouriteTeam <- NULL
lda_data$X <- NULL
head(lda_data) # display the first six rows

# ----- 6. Data Cleaning and pre-processing -----

lda_data$MemberClub <- as.factor(lda_data$MemberClub)
# Convert target variable to factor if it's not already

lda_data$FanGroup <- as.factor(lda_data$FanGroup) # where 'category' is 'local' or 'visiting' team supporter

# check variables are correctly defined

str(lda_data)

# ----- 7. Exploratory Data Analysis -----
pairs(lda_data[,1:6], col=lda_data$FanGroup)  # Pairwise plots

library(ggplot2)

# Boxplot for Age
ggplot(lda_data, aes(x = FanGroup, y = Age, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Age by Fan Group", x = "Fan Group", y = "Age")

# Boxplot for YearsAsFan
ggplot(lda_data, aes(x = FanGroup, y = YearsAsFan, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Years As Fan by Fan Group", x = "Fan Group", y = "Years As Fan")

# Boxplot for MatchesAttended
ggplot(lda_data, aes(x = FanGroup, y = MatchesAttended, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Matches Attended by Fan Group", x = "Fan Group", y = "Matches Attended")

# Boxplot for MerchandiseSpending
ggplot(lda_data, aes(x = FanGroup, y = MerchandiseSpending, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Merchandise Spending by Fan Group", x = "Fan Group", y = "Merchandise Spending")

# Bar Plot for MemberClub
ggplot(lda_data, aes(x = MemberClub, fill = FanGroup)) +
  geom_bar(position = "dodge") +
  labs(title = "Member Club by Fan Group", x = "Member Club", y = "Count")


# ----- 8. Splitting the Data -----
set.seed(123) # for reproducibility
trainIndex <- sample(1:nrow(lda_data), 0.8 * nrow(lda_data)) # 80% for training
trainData <- lda_data[trainIndex, ]
testData <- lda_data[-trainIndex, ]


# ----- 9. Performing LDA -----

# note that here, I am manually inputting the variables into the model. later, I will use code that includes ALL variables without having to specify them.
ldaModel <- lda(FanGroup ~ Age + YearsAsFan + MatchesAttended + MerchandiseSpending + MemberClub, data=trainData)
print(ldaModel)

# ----- 10. Performing Model Evaluation -----
ldaPredict <- predict(ldaModel, testData)
table(ldaPredict$class, testData$FanGroup)

# Calculate accuracy
mean(ldaPredict$class == testData$FanGroup)

# ----- 11. Diagnostics and Interpretation -----

ldaModel$scaling

# Confusion Matrix
confusionMatrix(ldaPredict$class, testData$FanGroup)

print(confusionMatrix)

lda_model <- lda(FanGroup ~ ., data = lda_data)

# Plotting the outcome of the model;
# Predict using the LDA model
lda_pred <- predict(lda_model)

# Add LDA components to the original data
lda_model <- cbind(lda_data, lda_pred$x)

# LDA Component 1 by Species
ggplot(lda_model, aes(FanGroup, LD1, fill = FanGroup)) +
  geom_boxplot() +
  ggtitle("Boxplot of LD1 by Species")


# ----- 12. Linear Discriminant Analysis: Practice -----

# Step One: Load Dataset

lda_data_02 <- read.csv('https://www.dropbox.com/scl/fi/p4hr96dtpgii50ufbn8oo/lda_02.csv?rlkey=jx7we1wggbb4n6xm8bqrllvay&dl=1')
head(lda_data_02) # display the first six rows

# Step Two: Cleaning and Pre-processing
lda_data_02$MemberClub <- as.factor(lda_data_02$MemberClub)

# Convert target variable to factor if it's not already
lda_data_02$FanGroup <- as.factor(lda_data_02$FanGroup) # where 'category' is 'local' or 'visiting' team supporter

# check variables are correctly defined
str(lda_data_02)


# Step Three: Exploratory Data Analysis

pairs(lda_data_02[,1:6], col=lda_data_02$FanGroup)  # Pairwise plots
library(ggplot2)

# Boxplot for Age
ggplot(lda_data_02, aes(x = FanGroup, y = Age, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Age by Fan Group", x = "Fan Group", y = "Age")

# Boxplot for YearsAsFan
ggplot(lda_data_02, aes(x = FanGroup, y = YearsAsFan, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Years As Fan by Fan Group", x = "Fan Group", y = "Years As Fan")

# Boxplot for MatchesAttended
ggplot(lda_data_02, aes(x = FanGroup, y = MatchesAttended, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Matches Attended by Fan Group", x = "Fan Group", y = "Matches Attended")

# Boxplot for MerchandiseSpending
ggplot(lda_data_02, aes(x = FanGroup, y = MerchandiseSpending, fill = FanGroup)) +
  geom_boxplot() +
  labs(title = "Merchandise Spending by Fan Group", x = "Fan Group", y = "Merchandise Spending")

# Bar Plot for MemberClub
ggplot(lda_data_02, aes(x = MemberClub, fill = FanGroup)) +
  geom_bar(position = "dodge") +
  labs(title = "Member Club by Fan Group", x = "Member Club", y = "Count")


# Step Four: Splitting the Data 

set.seed(123) # for reproducibility
trainIndex <- sample(1:nrow(lda_data_02), 0.8 * nrow(lda_data_02)) # 80% for training
trainData_02 <- lda_data_02[trainIndex, ]
testData_02 <- lda_data_02[-trainIndex, ]


# Step Five: Performing LDA

ldaModel_02 <- lda(FanGroup ~ Age + YearsAsFan + MatchesAttended + MerchandiseSpending + SocialMediaEngagement + MemberClub, data=trainData_02)
print(ldaModel_02)

# Step Six: Model Evaluation

ldaPredict_02 <- predict(ldaModel_02, testData_02)
table(ldaPredict_02$class, testData_02$FanGroup)

mean(ldaPredict_02$class == testData_02$FanGroup)

# Step Seven: Diagnostics and Interpretation

ldaModel_02$scaling

# Confusion Matrix
confusionMatrix(ldaPredict_02$class, testData_02$FanGroup)

print(confusionMatrix)

lda_model_02 <- lda(FanGroup ~ ., data = lda_data_02)

# Plotting the outcome of the model;
# Predict using the LDA model
lda_pred_02 <- predict(lda_model_02)

# Add LDA components to the original data
lda_model_02 <- cbind(lda_data_02, lda_pred_02$x)

# LDA Component 1 by Species
ggplot(lda_model_02, aes(FanGroup, LD1, fill = FanGroup)) +
  geom_boxplot() +
  ggtitle("Boxplot of LD1 by Species")

# LDA Component 2 by Species
ggplot(lda_data_02, aes(FanGroup, LD2, fill = FanGroup)) +
  geom_boxplot() +
  ggtitle("Boxplot of LD2 by Species")




