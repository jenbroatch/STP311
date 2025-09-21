# Module 4 Power Transformations 


# Install packages once if needed:
# install.packages(c("MASS", "car"))

library(MASS)  # for boxcox
library(car)   # optional: additional diagnostics


forbes <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP311/refs/heads/master/DataSets/forbes.csv", header=F)

# Assign variable names
names(forbes) <- c("boilpt", "baropres")
attr(forbes$boilpt,  "label") <- "Boiling Point of Water"
attr(forbes$baropres,"label") <- "Barometric Pressure"


title <- "All data included"
plot(forbes$boilpt, forbes$baropres,
     xlab = "Boiling Point of Water", ylab = "Barometric Pressure",
     main = paste("Forbes:", title), pch = 1)

# ---- PROC REG with OUTPUT (predicted, residuals, studentized) ----
m1 <- lm(baropres ~ boilpt, data = forbes)
summary(m1)

# Create "residout" like SAS OUTPUT statement
residout <- data.frame(
  pred     = fitted(m1),
  resid    = resid(m1),
  rstudent = rstudent(m1),
  boilpt   = forbes$boilpt,
  baropres = forbes$baropres
)

# RStudent vs Predicted (PROC GPLOT)
plot(residout$pred, residout$rstudent,
     xlab = "Predicted", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Predicted", pch = 1)
abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = "gray40")

# Normality plot for residuals 
qqnorm(residout$resid, main = "QQ-Plot of Residuals (Forbes all data)")
qqline(residout$resid, col = "red")

forbes2 <- forbes[-12, ] 

m2 <- lm(baropres ~ boilpt, data = forbes2)
summary(m1)

residout2 <- data.frame(
  pred     = fitted(m2),
  resid    = resid(m2),
  rstudent = rstudent(m2),
  boilpt   = forbes2$boilpt,
  baropres = forbes2$baropres
)

# RStudent vs Predicted (PROC GPLOT)
plot(residout2$pred, residout2$rstudent,
     xlab = "Predicted", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Predicted", pch = 1)
abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = "gray40")

#Like Transreg is SAS - Box Cox Power Transformation
boxcox(m2, lambda = seq(-2, 2, 0.05))

bc <- boxcox(m2, lambda = seq(-2, 2, 0.05), plotit = FALSE)
lambda_best <- bc$x[which.max(bc$y)]
lambda_best 
# Select "best from interval


forbes3 <- forbes2
forbes3$lbaro <- log(forbes3$baropres)

m3 <- lm(lbaro ~ boilpt, data = forbes3)

summary(m3)

residout3 <- data.frame(
  pred     = fitted(m3),
  resid    = resid(m3),
  rstudent = rstudent(m3),
  boilpt   = forbes3$boilpt,
  lbaro    = forbes3$lbaro
)



# Plot studentized residuals vs predicted (like PROC GPLOT with RStudentByPredicted)
plot(residout3$pred, residout3$rstudent,
     xlab = "Predicted", ylab = "Studentized Residuals",
     main = "Studentized Residuals vs Predicted (log model, no outlier)",
     pch = 1)
abline(h = c(-2, 0, 2), col = "gray40", lty = c(2, 1, 2))

# Normal Q-Q plot for residuals 
qqnorm(residout3$resid, main = "QQ-Plot of Residuals (log model, no outlier)")
qqline(residout3$resid, col = "red")

