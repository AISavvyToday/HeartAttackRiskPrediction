# Load necessary libraries

library(dplyr)
library(ggplot2)
# library(tidyverse)


heart_attack_prediction_dataset <- read.csv("heart_attack_prediction_dataset.csv")
# Inspect the data for any missing values or anomalies
summary(heart_attack_prediction_dataset)
str(heart_attack_prediction_dataset)


# Clean the data (Remove rows with any NA values)
heart_attack_prediction_dataset_clean <- na.omit(heart_attack_prediction_dataset)


# Check for any impossible or out-of-range values for numeric variables
heart_attack_prediction_dataset_clean <- heart_attack_prediction_dataset_clean %>%
  filter(
    Age > 0 & Age < 120,
    Cholesterol > 0,
    `Heart.Rate` > 0 & `Heart.Rate` < 220,
    `Exercise.Hours.Per.Week` >= 0 & `Exercise.Hours.Per.Week` <= 168,
    `Stress.Level` >= 0 & `Stress.Level` <= 10,
    `Sedentary.Hours.Per.Day` >= 0 & `Sedentary.Hours.Per.Day` <= 24,
    `Income` > 0,
    `BMI` > 0,
    `Triglycerides` > 0,
    `Physical.Activity.Days.Per.Week` >= 0 & `Physical.Activity.Days.Per.Week` <= 7,
    `Sleep.Hours.Per.Day` >= 0 & `Sleep.Hours.Per.Day` <= 24
  )



# Check for any invalid categories or typos in categorical variables
heart_attack_prediction_dataset_clean <- heart_attack_prediction_dataset_clean %>%
  mutate(
    Sex = factor(Sex),
    `Blood.Pressure` = factor(`Blood.Pressure`),
    Diabetes = factor(`Diabetes`),
    `Family.history` = factor(`Family.History`),
    Smoking = factor(Smoking),
    Obesity = factor(Obesity),
    `Alcohol.Consumption` = factor(`Alcohol.Consumption`),
    Diet = factor(Diet),
    `Previous.Heart.Problems` = factor(`Previous.Heart.Problems`),
    `Medication.Use` = factor(`Medication.Use`),
    Country = factor(Country),
    Continent = factor(Continent),
    Hemisphere = factor(Hemisphere),
    `Heart.Attack.Risk` = factor(`Heart.Attack.Risk`)
  )


# Check the cleaned data
summary(heart_attack_prediction_dataset_clean)
str(heart_attack_prediction_dataset_clean)


# 1. What is the distribution of blood pressure levels in the dataset, and is there a correlation between high blood pressure and the risk of a heart attack?
# Histogram for Blood Pressure Distribution

# Preprocessing of the blood pressure data that is in in 'systolic/diastolic' format
heart_attack_prediction_dataset_clean$Blood.Pressure <- as.numeric(gsub("/.*", "", heart_attack_prediction_dataset_clean$Blood.Pressure))


ggplot(heart_attack_prediction_dataset_clean, aes(x = Blood.Pressure)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Blood Pressure Levels", x = "Blood Pressure", y = "Frequency")


# Scatter Plot for Blood Pressure vs. Heart Attack Risk
heart_attack_prediction_dataset_clean$Blood.Pressure <- as.numeric(gsub("/.*", "", heart_attack_prediction_dataset_clean$Blood.Pressure))
ggplot(heart_attack_prediction_dataset_clean, aes(x = Blood.Pressure, y = Heart.Attack.Risk)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Blood Pressure vs. Heart Attack Risk", x = "Blood Pressure", y = "Heart Attack Risk")



# 2.Are there any significant differences in cholesterol levels between individuals who have experienced a heart attack and those who haven't?

cholesterol <- heart_attack_prediction_dataset_clean[["Cholesterol"]]
heart_attack_risk <- heart_attack_prediction_dataset_clean[["Heart.Attack.Risk"]]

# Boxplot for Cholesterol Levels by Heart Attack Risk
ggplot(heart_attack_prediction_dataset_clean, aes(x = as.factor(Heart.Attack.Risk), y = Cholesterol)) +
  geom_boxplot() +
  labs(title = "Cholesterol Levels by Heart Attack Risk", x = "Heart Attack Risk", y = "Cholesterol Level")





# 3.How does heart rate affect the risk of a heart attack? Is there an optimal heart rate range for lower risk?


heart_rate <- heart_attack_prediction_dataset_clean[["Heart.Rate"]]

# Scatter Plot for Heart Rate vs. Heart Attack Risk
ggplot(heart_attack_prediction_dataset_clean, aes(x = Heart.Rate, y = as.factor(Heart.Attack.Risk))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Heart Rate vs. Heart Attack Risk", x = "Heart Rate", y = "Heart Attack Risk")





#4.What is the prevalence of diabetes in the dataset, and is there a clear association between diabetes and an increased risk of heart attack?


diabetes <- heart_attack_prediction_dataset_clean[["Diabetes"]]

# Bar Plot for Diabetes Prevalence
diabetes <- heart_attack_prediction_dataset_clean[["Diabetes"]]
ggplot(heart_attack_prediction_dataset_clean, aes(x = as.factor(Diabetes))) +
  geom_bar() +
  labs(title = "Prevalence of Diabetes", x = "Diabetes", y = "Count")

# Boxplot for Diabetes vs. Heart Attack Risk
ggplot(heart_attack_prediction_dataset_clean, aes(x = as.factor(Diabetes), y = Heart.Attack.Risk)) +
  geom_boxplot() +
  labs(title = "Diabetes vs. Heart Attack Risk", x = "Diabetes", y = "Heart Attack Risk")




#5.How does age factor into heart attack risk? Is there a particular age group that is more susceptible to heart attacks?

age <- heart_attack_prediction_dataset_clean[["Age"]]

# Histogram for Age Distribution
ggplot(heart_attack_prediction_dataset_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Scatter Plot for Age vs. Heart Attack Risk
ggplot(heart_attack_prediction_dataset_clean, aes(x = Age, y = as.factor(Heart.Attack.Risk))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Age vs. Heart Attack Risk", x = "Age", y = "Heart Attack Risk")

