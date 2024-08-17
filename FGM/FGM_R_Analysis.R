# Set Working Directory and Load Data
setwd("/Users/naima.abdirahman/Downloads")
fgm.dat <- read.csv("FGM Survey data - Sheet1.csv")

# Data Cleaning
# Remove "No" from FGM status and filter people under 12
subFGM <- fgm.dat[fgm.dat$FGM_Status != "No",]
fgm2.dat <- subFGM[subFGM$Current_Age > 12, ]

# Convert variables to factors
fgm2.dat$LocationClean <- as.factor(fgm2.dat$Location)
fgm2.dat$EducationLevelClean <- as.factor(fgm2.dat$Education_Level)
fgm2.dat$SupportPracticeClean <- as.factor(fgm2.dat$Support_Practice)

# Summarize Data
dim(fgm2.dat)
summary(fgm2.dat)
table(fgm2.dat$Support_Practice)
table(fgm2.dat$FGM_Status)

# Visualizations
# Boxplot of FGM Age by Support Practice
boxplot(fgm2.dat$FGM_Age ~ fgm2.dat$Support_Practice,
        xlab = 'Supports Practice',
        ylab = 'Age of FGM')

# Bar Plot of Support Practice by Location
library(ggplot2)
ggplot(fgm2.dat, aes(x = Support_Practice, fill = LocationClean)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(x = 'Supports Practice', y = 'Frequency', fill = 'Location') +
  theme_minimal() +
  theme(legend.text = element_text(size = 20), axis.text.x = element_text(size = 20, color = "black"))

# Faceted Bar Plot of Education Level and Location
ggplot(fgm2.dat, aes(x = LocationClean, fill = SupportPracticeClean)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = 'Location', y = 'Frequency', fill = 'Supports Practice') +
  theme_minimal() +
  facet_grid(EducationLevelClean ~ .)

# Logistic Regression Models
# Fit basic logistic regression model
logit_model <- glm(SupportPracticeClean ~ EducationLevelClean + LocationClean + Current_Age, 
                   data = fgm2.dat, 
                   family = binomial(link='logit'))
summary(logit_model)

# Model with Interaction Terms
logit_model_interaction <- glm(SupportPracticeClean ~ EducationLevelClean * LocationClean + Current_Age, 
                                data = fgm2.dat, 
                                family = binomial)
summary(logit_model_interaction)

# Odds Ratios
exp(coef(logit_model))
exp(coef(logit_model_interaction))

# Cross-Validation
# Install and load packages
install.packages(c("caret", "pscl", "ModelMetrics"))
library(caret)
library(ModelMetrics)
library(pscl)

# Define PRESS Function
PRESS <- function(logistic.model) {
  pred_prob <- predict(logistic.model, type = "response")
  pr <- residuals(logistic.model) / (pred_prob * (1 - pred_prob))
  PRESS <- sum(pr^2)
  return(PRESS)
}

# Perform Cross-Validation for Different Models
k <- 10
folds <- createFolds(fgm2.dat$SupportPracticeClean, k = k)

avg_rmse <- numeric()
models <- list(
  "Current_Age" = glm(SupportPracticeClean ~ Current_Age, data = fgm2.dat, family = binomial),
  "Current_Age + EducationLevelClean" = glm(SupportPracticeClean ~ Current_Age + EducationLevelClean, data = fgm2.dat, family = binomial),
  "Current_Age + EducationLevelClean + LocationClean" = glm(SupportPracticeClean ~ Current_Age + EducationLevelClean + LocationClean, data = fgm2.dat, family = binomial),
  "Interaction Terms" = glm(SupportPracticeClean ~ EducationLevelClean * LocationClean + Current_Age, data = fgm2.dat, family = binomial)
)

for (model_name in names(models)) {
  model <- models[[model_name]]
  results <- numeric()
  for (i in 1:k) {
    train <- fgm2.dat[-folds[[i]], ]
    validate <- fgm2.dat[folds[[i]], ]
    fit_mod <- update(model, data = train)
    predictions <- predict(fit_mod, newdata = validate, type = "response")
    accuracy <- rmse(predictions, validate$SupportPracticeClean)
    results <- c(results, accuracy)
  }
  avg_rmse[model_name] <- mean(results)
}

# Print Average RMSE for Each Model
print(avg_rmse)

# Confusion Matrix and Performance Metrics
logit_model <- glm(SupportPracticeClean ~ Current_Age, data = fgm2.dat, family = binomial)
predictedSupport <- predict(logit_model, newdata = fgm2.dat, type = 'response')

# Convert to binary
fgm2.dat$default <- ifelse(fgm2.dat$SupportPracticeClean == "Yes", 1, 0)
confusionTable <- table(predictedSupport > 0.5, fgm2.dat$default)
print(confusionMatrix(confusionTable))

# Performance Metrics
sensitivity(factor(fgm2.dat$default), factor(predictedSupport > 0.5))
specificity(factor(fgm2.dat$default), factor(predictedSupport > 0.5))
posPredValue(factor(predictedSupport > 0.5), factor(fgm2.dat$default))
accuracy <- mean((predictedSupport > 0.5) == fgm2.dat$default)
print(accuracy)

# Likelihood Ratio Test
null_model <- glm(SupportPracticeClean ~ 1, data = fgm2.dat, family = binomial())
lrt <- anova(null_model, logit_model, test = "LRT")
print(lrt)

# Linearity Check
plot(logit_model$fitted.values, fgm2.dat$Current_Age, xlab = "Fitted log odds", ylab = "Current Age", main = "Scatterplot of Current Age vs. Log Odds")
lines(lowess(logit_model$fitted.values, fgm2.dat$Current_Age), col = "red")
