# Library ----

library(s20x)
library(tidyverse) # for easy data manipulation and visualization
library(here)
library(ggpubr)
library(tidymodels) # for the rsample package, along with the rest of tidymodels
library(modeldata) # for the cells data
library(MASS)
library(pROC)

# Data upload----

df <- read_csv("Data/UCI_Credit_Card.csv") %>% 
  mutate(SEX = factor(SEX),
         EDUCATION = factor(EDUCATION),
         MARRIAGE = factor(MARRIAGE))#,
         #default.payment.next.month = factor(default.payment.next.month))

# Data Check----

dim(df)

head(df)
 
# Data splitting----
# https://www.listendata.com/2015/02/splitting-data-into-training-and-test.html

set.seed(123)
df_split <- sort(sample(nrow(df), nrow(df)*0.7))

df_split

df.train <- df[df_split,]
df.test <- df[-df_split,]

df.train
df.test

# Model

glm(default.payment.next.month ~., data = df[,-1]) %>% 
  summary()

model <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + MARRIAGE + AGE + 
               PAY_0 + BILL_AMT1 + PAY_AMT1, data = df[,-1], family = "binomial") 
summary(model)

df$Pred <- ifelse(predict(model, type = "response")> 0.8, "Positive", "Negative")

table(df$default.payment.next.month, df$Pred)

# Evaluation of Classification----
# http://www.sthda.com/english/articles/36-classification-methods-essentials/143-evaluation-of-classification-model-accuracy-essentials/

## Fit LDA ----
# fit the LDA model on the training set and make predictions on the test data

fit <- lda(default.payment.next.month ~ ., data = df.train)

## Prediction_test data----
# make prediction on the test data

predictions <- predict(fit, df.test)

prediction.probabilities <- predictions$posterior[,2]
predicted.classes <- predictions$class
observed.classes <- df.test$default.payment.next.month

## Overall classification accuracy----

accuracy <- mean(observed.classes == predicted.classes)
accuracy

error <- mean(observed.classes != predicted.classes)
error

### From the output above, the linear discriminant analysis correctly predicted the individual outcome in 81% of the cases. 
### This is by far better than random guessing. The misclassification error rate can be calculated as 100 - 81% = 19%.

## Confusion matrix----
# 0 is negative and 1 is positive

# number of cases
table(observed.classes, predicted.classes)

# proportion of cases
table(observed.classes, predicted.classes) %>% 
  prop.table() %>% 
  round(digits = 3)

## Precision, Recall and Specificity----

confusionMatrix(predicted.classes, observed.classes)

# ROC curve----

# Compute roc
res.roc <- roc(observed.classes, prediction.probabilities)
plot.roc(res.roc, print.auc = TRUE)

# Extract some interesting result

roc.data <- tibble(
                    thresholds = res.roc$thresholds,
                    sensitivity = res.roc$sensitivities,
                    specificity = res.roc$specificities
                  )

# Get the probability threshold for specificity = 0.6

roc.data %>% filter(specificity >= 0.6)

plot.roc(res.roc, print.auc = TRUE, print.thres = 'best')






















