## Github repo: https://github.com/sametha-s/stroke_prediction_logit

library(tidyverse)
library(gt)
library(finalfit)
library(ggplot2)
library(car)


## Load dataset
stroke <- read.csv("healthcare-dataset-stroke-data.csv")

View(stroke)
glimpse(stroke)
summary(stroke)

## Select relevant columns
stroke_s <- stroke %>%
  select (
    stroke, 
    gender,
    age,
    hypertension,
    heart_disease,
    avg_glucose_level,
    bmi,
    smoking_status
  )

View(stroke_s)

# Check class of columns 
sapply(stroke_s, class)

## Handle missing/irrelevant values
stroke_cc <- stroke_s %>%
  mutate(
    bmi = as.numeric(bmi)
  )

sapply(stroke_cc, class)
colSums(is.na(stroke_cc)) # 201 NA's in bmi column

stroke_cc <- stroke_cc %>%
  drop_na(stroke, 
          age, 
          hypertension, 
          heart_disease, 
          avg_glucose_level,
          bmi
          )

View(stroke_cc) # remaining 4909 observations

# One row with "Other" gender <- drop
stroke_cc <- stroke_cc[stroke_cc$gender != "Other", ]
View(stroke_cc)

## Transform qualitative variables into factors
stroke_cc <- stroke_cc %>%
  mutate(
    stroke = factor(stroke),
    gender = factor(gender),
    hypertension = factor(hypertension),
    heart_disease = factor(heart_disease),
    smoking_status = factor(smoking_status)
  )

View(stroke_cc)
nrow(stroke_cc) # 4908 observations
summary(stroke_cc)

sapply(stroke_cc, levels)
# stroke; no stroke, 1: stroke
# gender; female, male
# hypertension; 0:no hp, 1:hp
# heart_disease; 0:no hd, 1:hd,
# smoking_status; formerly smoked, never smoked, smokes, Unknown

# Reset row names to be 1:n
rownames(stroke_cc) <- NULL
View(stroke_cc)

# Change reference level of smoking_status from "formerly smoked: to "never smoked"
# Represents baseline (lowest risk)
stroke_cc$smoking_status <- relevel(stroke_cc$smoking_status,
                                    ref = "never smoked")

## Full model
model <- glm(stroke ~ . ,
             data = stroke_cc,
             family = "binomial",
             )

# print results
summary(model)
# smoking_status "formerly smoked" , "unknown" not significant
# smoking status "smokes" marginally significant
# bmi not significant
# heart disease marginally significant
# do not drop variables as they are known risk factors of stroke

## Reduced model (Removing all marginally/non significant variables)
model_reduced <- glm(stroke ~ gender + age + hypertension + avg_glucose_level,
                     data = stroke_cc,
                     family = "binomial")

summary(model_reduced)


## Compare reduced with full model (LRT)
# H0: Reduced model fits as well as full model (No significant difference between models)
# H1: The full model significant improves model fit
anova(model_reduced, model, test = "Chisq") 
# Pr > 0.05 : Fail to reject H0 
# The full model fits just as well as the reduced model
# Removing marginally/non significant variables did not significantly improve model fit (LRT p > 0.05)
# Although LRT Pr > 0.05, still use full model as variables are known risk factors of stroke

## Odds Ratio (OR) & 95% CI (Full model)
round(exp(cbind(OR = coef(model), confint(model))), 3)
# gender: odds of stroke for males are 0.989 times the odds for females
# age: odds of stroke are multiplied by a factor of 1.071 for each 1 unit increase in age
# hypertension: odds of stroke are 1.678 more likely if hp = 1
# heart_disease: not significant as 1 is included in CI
# bmi: not significant as 1 is included in CI
# smoking_status: not significant as 1 is included in CI


## Visualization

## Bar plots: stroke prevalence by categorical predictors

# gender
stroke_cc %>%
  count(gender, stroke) %>%
  group_by(gender) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(gender, prop, fill = factor(stroke))) +
  geom_col(position = "dodge") +
  labs(y = "Proportion with/without stroke",
       fill = "Stroke") +
  theme_minimal()

# hypertension
stroke_cc %>%
  count(hypertension, stroke) %>%
  group_by(hypertension) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(factor(hypertension), prop, fill = factor(stroke))) +
  geom_col(position = "dodge") +
  labs(x = "Hypertension", y = "Proportion", fill = "Stroke") +
  theme_minimal()

# heart_disease
stroke_cc %>%
  count(heart_disease, stroke) %>%
  group_by(heart_disease) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(factor(heart_disease), prop, fill = factor(stroke))) +
  geom_col(position = "dodge") +
  labs(x = "Heart disease", y = "Proportion", fill = "Stroke") +
  theme_minimal()

# smoking_status
stroke_cc %>%
  count(smoking_status, stroke) %>%
  group_by(smoking_status) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(smoking_status, prop, fill = factor(stroke))) +
  geom_col(position = "dodge") +
  labs(y = "Proportion", fill = "Stroke") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Boxplots of continuous vars by stroke (0/1)

# age
ggplot(stroke_cc, aes(x = factor(stroke), y = age)) +
  geom_boxplot() +
  labs(x = "Stroke", y = "Age") +
  theme_minimal()

# avg_glucose_level
ggplot(stroke_cc, aes(x = factor(stroke), y = avg_glucose_level)) +
  geom_boxplot() +
  labs(x = "Stroke", y = "Average glucose level") +
  theme_minimal()

# bmi
ggplot(stroke_cc, aes(x = factor(stroke), y = bmi)) +
  geom_boxplot() +
  labs(x = "Stroke", y = "BMI") +
  theme_minimal()

# Visualize Odds Ratio & 95% CI
dependent <- "stroke"
independent <- c("gender", "age", "hypertension", "heart_disease", 
                 "avg_glucose_level", "bmi", "smoking_status")

stroke_cc %>%
  or_plot(dependent, independent, 
          table_text_size = 3.5)


## Assumption checking

# 1. Linearity of continuous variables
stroke_cc %>%
  select(age, avg_glucose_level, bmi) %>%
  mutate(log_odds = predict(model)) %>%
  pivot_longer(-log_odds) %>%
  ggplot(aes(log_odds, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name)

# 2. Multicollinearity 
vif(model)
# All variables have a VIF < 5
# Multicollinearity assumption is verified






