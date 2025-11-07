
#Module 5 Model Selection 


library(dplyr)
college_raw<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP311/master/DataSets/collegemajor538.csv")
college <- college_raw %>%
  select(median, sharewomen, unemployment_rate, college_jobs, non_college_jobs, major_category) %>%
  filter(!is.na(median)) %>%
  mutate(major_category = as.factor(major_category))


# quick glance
summary(college)
View(college)

library(MASS)
# Full model with categorical major_category
full_college <- lm(median ~ sharewomen+ unemployment_rate+college_jobs+ non_college_jobs+ major_category, data = college)
Partial_model <- lm(median ~ sharewomen+college_jobs+ major_category, data = college)
summary(Partial_model)
step_college_aic <- stepAIC(full_college, direction = "both", trace = FALSE)
summary(step_college_aic)


#LASSO
library(glmnet)
# Create model matrix
x <- model.matrix(median ~ ., college)[, -1]  # predictors (remove intercept)
y <- college$median                           # response variable

#Using LAR for optimal lambda
library(lars)
lar_fit <- lars(x, y, type = "lasso")
summary(lar_fit)


plot(lar_fit)
title("LASSO Path via Least Angle Regression")

cv_lar <- cv.lars(x, y, type = "lasso", K = 10)
plot(cv_lar)

coef(cv_lasso, s = "lambda.min")

#Alternative method - using lambda.min - equivalent to LAR
cv_lasso <- cv.glmnet(x, y, alpha = 1)  # alpha = 1 for LASSO
coef(cv_lasso, s = "lambda.min")


plot(cv_lasso)
# Predict and compute RMSE
preds <- predict(cv_lasso, newx = x, s = "lambda.min")
rmse <- sqrt(mean((y - preds)^2))
rmse

fit_lasso <- glmnet(x, y, alpha = 1)
plot(fit_lasso, xvar = "lambda", label = TRUE)

install.packages("olsrr")
library(olsrr)

# Full model
full_model <- lm(median ~ ., data = college)

# Stepwise selection using p-values
step_p_model <- ols_step_both_p(full_model,
                                pent = 0.05,   # p-value to enter
                                prem = 0.10,   # p-value to remove
                                details = TRUE)


step_p_model$model

# We’ll compare the suggested models using Cross- Validation 
model_basic <- lm(median ~ sharewomen , data = college)
model_stepp <- lm(median ~ sharewomen+major_category , data = college) #by stepwise with p-value
model3_aic <- lm(median ~ sharewomen+major_category+college_jobs + non_college_jobs, data = college)


set.seed(722)
train_control <- trainControl(method = "cv", number = 5)

# Train using CV
cv_basic <- train(median ~ sharewomen, data = college, method = "lm", trControl = train_control, metric = "RMSE")
cv_stepp <- train(median ~ sharewomen+major_category, data = college, method = "lm", trControl = train_control, metric = "RMSE")
cv_aic <- train(median ~ sharewomen+major_category+college_jobs + non_college_jobs, data = college, method = "lm", trControl = train_control, metric = "RMSE")
cv_LASSO <-train(median ~ sharewomen+major_category+college_jobs + non_college_jobs+ + unemployment_rate, data = college, method = "lm", trControl = train_control, metric = "RMSE")
cv_basic
cv_stepp
cv_aic
cv_LASSO






