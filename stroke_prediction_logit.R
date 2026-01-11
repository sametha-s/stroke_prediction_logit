## Github repo: https://github.com/sametha-s/stroke_prediction_logit

library(tidyverse)
library(gt)
library(finalfit)
library(ggplot2)
library(car)
library(pROC)


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
nrow(stroke_cc)
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
model_reduced <- glm(stroke ~ age + hypertension + avg_glucose_level,
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

stroke_cc_bt <- stroke_cc
stroke_cc_bt$stroke_num <- as.numeric(as.character(stroke_cc_bt$stroke))

bt_out <- boxTidwell(
  stroke_num ~ age + avg_glucose_level + bmi,
  other.x = ~ gender + hypertension + heart_disease + smoking_status,
  data    = stroke_cc_bt
)

bt_out 

# 2. Multicollinearity 
vif(model)
# All variables have a VIF < 5
# Multicollinearity assumption is verified

########################### PREDICTION #########################################
set.seed(234)

# Generate random indices for the training and testing sets
train_indices <- sample(nrow(stroke_cc), 0.7 * nrow(stroke_cc))   # 70% training
test_indices <- setdiff(1:nrow(stroke_cc), stroke_train_indices)  # 30% testing

# Create training and testing sets using indices
train_data <- stroke_cc[train_indices, ]
test_data <- stroke_cc[test_indices, ]

# Check size of testing and training datasets
nrow(train_data)
nrow(test_data)

####### TRAIN MODEL TO PREDICT STROKE ##########################################
stroke_predict_model <- glm(stroke ~ . , 
                          data = train_data,
                          family = "binomial")

####### PREDICT STROKE USING MODEL #############################################
stroke_predict <- predict(stroke_predict_model, test_data, type = "response")

############## 0.5 THRESHOLD ##################################################
pred_class <- ifelse(stroke_predict > 0.5, 1, 0)

# CHECK MODEL PERFORMANCE AT 0.5
# confusion matrix
table(Predicted = pred_class, Actual = test_data$stroke)

# accuracy
mean(pred_class == test_data$stroke)


thresholds <- c(0.3, 0.2, 0.14, 0.1)

get_row <- function(t) {
  pred <- ifelse(stroke_predict > t, 1, 0)
  TN <- sum(pred == 0 & test_data$stroke == 0)
  TP <- sum(pred == 1 & test_data$stroke == 1)
  FP <- sum(pred == 1 & test_data$stroke == 0)
  FN <- sum(pred == 0 & test_data$stroke == 1)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  data.frame(threshold = t, TP, FN, FP, TN, sensitivity, specificity)
}

summary_tbl <- do.call(rbind, lapply(thresholds, get_row))
summary_tbl

####### 0.1 THRESHOLD ##########################################################
new_pred_class <- ifelse(stroke_predict > 0.1, 1, 0)

## Check model performance at 0.1
# confusion matrix
table(Predicted = new_pred_class, Actual = test_data$stroke)

# accuracy
mean(new_pred_class == test_data$stroke)

############### AUC ############################################################
roc_obj <- roc(test_data$stroke, stroke_predict)
auc(roc_obj)


############# PREDICT NEW PATIENTS #############################################
# assume new_patients has the same predictor columns as stroke_cc (except stroke)
new_patients <- data.frame(
  gender            = c("Male", "Female", "Female"),
  age               = c(55, 72, 40),
  hypertension      = factor(c(1, 0, 0)),
  heart_disease     = factor(c(0, 1, 0)),
  avg_glucose_level = c(110, 180, 95),
  bmi               = c(27.5, 31.2, 23.0),
  smoking_status    = c("formerly smoked", "never smoked", "smokes"),
  stringsAsFactors  = TRUE
)
new_prob  <- predict(stroke_predict_model, newdata = new_patients, type = "response")
new_class <- ifelse(new_prob > 0.1, 1, 0)

new_prob
new_class









