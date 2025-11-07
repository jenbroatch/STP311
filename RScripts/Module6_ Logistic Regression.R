#Module 6: Logistic Regression 

# Logistic regression (event = 1, same as "descending" in SAS)
prostate <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP311/refs/heads/master/DataSets/NodalINV.csv", header=F)

names(prostate) <- c("case", "age", "acid", "xray", "size", "grade", "nodalinv" )
prostate$lacd = log(prostate$acid)


fit <- glm(nodalinv ~ lacd + as.factor(xray) + as.factor(size) + as.factor(grade) + age,
           data = prostate,
           family = binomial(link = "logit"))

summary(fit)

# Odds ratios with 95% confidence intervals
exp(cbind(OR = coef(fit), confint(fit)))

#Like the Global test in SAS
fit_null <- glm(nodalinv ~ 1, data = prostate, family = binomial)

anova(fit_null, fit, test = "Chisq")

# Confidence intervals (profile likelihood, like plcl/plrl in SAS)
confint(fit)

# Wald confidence intervals (like waldcl/waldrl)
confint.default(fit)

# Predicted probabilities and deviance residuals (like OUTPUT statement)
prostate$predicted <- predict(fit, type = "response")
prostate$residual  <- residuals(fit, type = "deviance")

head(prostate)

# ROC curve (like OUTROC=roc in SAS)
roc_obj <- roc(prostate$nodalinv, prostate$predicted, ci = TRUE)
plot(roc_obj, main = "ROC Curve for NODALINV Logistic Model")

# Classification table (like ctable option in SAS)
threshold <- 0.5
pred_class <- ifelse(prostate$predicted > threshold, 1, 0)
table(Observed = prostate$nodalinv, Predicted = pred_class)

# Influence diagnostics (like influence/iplots in SAS)
influence_measures <- influence.measures(fit)
summary(influence_measures)
plot(hatvalues(fit), main = "Leverage (Hat values)", ylab = "Hat value")