#Module 2
#Simple Linear Regression 

# 1) Simulate strictly linear data
set.seed(42)
n <- 200
x <- runif(n, -3, 3)
y <- 2 * x + rnorm(n, sd = 1)      # no quadratic term!

# 2) Fit a linear model
model       <- lm(y ~ x)
fitted_vals <- fitted(model)
residuals   <- resid(model)

# 3) Draw a 1×2 panel: left = y vs x, right = residuals vs fitted
par(mfrow = c(1, 2),               # 1 row, 2 columns
    mar   = c(5, 4, 4, 2) + 0.1)   # margins: bottom, left, top, right

# Left: scatter of y against x
plot(x, y,
     pch  = 19,
     col  = "darkorange",
     xlab = "Predictor (x)",
     ylab = "Response (y)",
     main = "Scatter Plot of y vs x\n(Linear Relationship)")

# Right: residuals vs. fitted values — should be random around zero
plot(fitted_vals, residuals,
     pch  = 19,
     col  = "darkorange",
     xlab = "Predicted Values (Fitted y)",
     ylab = "Residuals (Observed y – Fitted y)",
     main = "Residuals vs \n Predicted Values")
abline(h = 0, lwd = 2)

# Restore single-plot layout
par(mfrow = c(1, 1))


# 1) Simulate data with a quadratic relationship
set.seed(42)
n  <- 200
x  <- runif(n, -3, 3)
y  <- 2*x + 0.5*x^2 + rnorm(n, sd = 1)

# 2) Fit a (misspecified) linear model
model       <- lm(y ~ x)
fitted_vals <- fitted(model)
residuals   <- resid(model)

# 3) Draw a 1×2 panel: left = y vs x, right = residuals vs fitted
par(mfrow = c(1, 2),               # 1 row, 2 columns
    mar   = c(5, 4, 4, 2) + 0.1)   # margins: bottom, left, top, right

# Left panel: scatter of y against x
plot(x, y,
     pch  = 19, 
     col  = "darkorange",
     xlab = "Predictor (x)",
     ylab = "Response (y)",
     main = "Scatter Plot of y vs x")

# Right panel: residuals vs. fitted values
plot(fitted_vals, residuals,
     pch  = 19, 
     col  = "darkorange",
     xlab = "Predicted Values (Fitted y)",
     ylab = "Residuals (Observed y – Fitted y)",
     main = "Residuals vs. \n Predicted Values")
abline(h = 0, col = "darkorange", lwd = 2)

# Restore default single‐plot layout (optional)
par(mfrow = c(1, 1))



# 1) Simulate data with a quadratic relationship
set.seed(42)
n <- 200
x <- runif(n, -3, 3)
y <- 2*x + 0.5*x^2 + rnorm(n, sd = 1)

# 2) Fit a simple linear model
model <- lm(y ~ x)

# 3) Extract fitted values and residuals
fitted_vals <- fitted(model)
resids      <- resid(model)

# 4) Plot residuals vs. fitted values
plot(
  fitted_vals, resids,
  pch    = 19,
  col    = "darkorange",
  xlab   = "Predicted Values (Fitted y)",
  ylab   = "Residuals (Observed y \u2013 Fitted y)",
  main   = "Residuals vs. Predicted Values (Nonlinear Pattern)"
)
abline(h = 0, col = "darkorange", lwd = 2)




# 1) Simulate heteroscedastic data
set.seed(123)
n <- 200
x <- runif(n, 0, 10)
# let the noise sd grow linearly with x
sigma <- 0.5 + 0.3 * x  
y     <- 2 + 1.5 * x + rnorm(n, mean = 0, sd = sigma)

# 2) Fit the linear model
model       <- lm(y ~ x)
fitted_vals <- fitted(model)
residuals   <- resid(model)

# 3) Side-by-side plots: scatter and residuals plot
par(mfrow = c(1, 2),               # 1 row, 2 columns
    mar   = c(5, 4, 4, 2) + 0.1)   # bottom, left, top, right margins

# Left panel: y vs x
plot(x, y,
     pch    = 19,
     col    = "darkorange",
     xlab   = "Predictor (x)",
     ylab   = "Response (y)",
     main   = "Scatter Plot of y vs x")

# Right panel: residuals vs fitted values
plot(fitted_vals, residuals,
     pch    = 19,
     col    = "darkorange",
     xlab   = "Predicted Values (Fitted y)",
     ylab   = "Residuals (Observed y \u2013 Fitted y)",
     main   = "Residuals vs \n Predicted Values")
abline(h = 0, lwd = 2)

# restore default layout
par(mfrow = c(1, 1))


# 1) Simulate predictor
set.seed(123)
n <- 200
x <- runif(n, 0, 10)

# 2) Case A: truly normal errors
y_norm <- 3 + 2*x + rnorm(n, sd = 1)
model_norm <- lm(y_norm ~ x)
res_norm   <- resid(model_norm)

# 3) Case B: right‐skewed errors (exponential)
y_skew <- 3 + 2*x + (rexp(n, rate = 1) - 1)  # subtract 1 to center roughly at 0
model_skew <- lm(y_skew ~ x)
res_skew   <- resid(model_skew)

# 4) Plot side by side QQ‐plots
par(mfrow = c(1, 2), 
    mar   = c(4, 4, 2, 1) + 0.1)  # adjust margins

# Left: QQ‐plot under Normal errors
qqnorm(res_norm,
       main = "Q-Q Plot of Residuals\n(Normal Errors)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Residuals")
qqline(res_norm, col = "steelblue", lwd = 2)

# Right: QQ‐plot under Skewed errors
qqnorm(res_skew,
       main = "Q-Q Plot of Residuals\n(Right Skewed Errors)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Residuals")
qqline(res_skew, col = "darkorange", lwd = 2)

# Reset layout
par(mfrow = c(1, 1))

# 1) Simulate linear data with constant variance, then insert one outlier
set.seed(2025)
n <- 200
x <- runif(n, 0, 10)
y <- 3 + 2 * x + rnorm(n, mean = 0, sd = 1)  # strict linear + constant‐sd noise

# Introduce a single outlier in y
# e.g., pick observation index 10 and add a large jump
y[10] <- y[10] + 5

# 2) Fit the linear model
model       <- lm(y ~ x)
fitted_vals <- fitted(model)
residuals   <- resid(model)

# 3) Side-by-side plots: scatter (with outlier) and residuals vs fitted (highlight outlier)
par(mfrow = c(1, 2),               # 1 row, 2 columns
    mar   = c(5, 4, 4, 2) + 0.1)   # margins: bottom, left, top, right

# Left panel: y vs x (outlier visible)
plot(x, y,
     pch    = 19,
     col    = "darkorange",
     xlab   = "Predictor (x)",
     ylab   = "Response (y)",
     main   = "Scatter Plot of y vs x\n(with One Outlier)")
points(x[10], y[10], col = "blue", pch = 19, cex = 1.3)  # highlight outlier
text(x[10], y[10] - 2, labels = "Outlier", col = "blue", cex = 0.8)

# Right panel: residuals vs fitted values (outlier’s residual stands out)
plot(fitted_vals, residuals,
     pch    = 19,
     col    = "darkorange",
     xlab   = "Predicted Values (Fitted y)",
     ylab   = "Residuals (Observed y – Fitted y)",
     main   = "Residuals vs Predicted Values\n(with Single Outlier)")
points(fitted_vals[10], residuals[10], col = "blue", pch = 19, cex = 1.3)
abline(h = 0, lwd = 2)

# Restore default layout
par(mfrow = c(1, 1))

# 4) QQ‐plot of residuals, highlighting the outlier
qqnorm(residuals,
       pch    = 19,
       col    = "darkorange",
       xlab   = "Theoretical Quantiles",
       ylab   = "Sample Residuals",
       main   = "Normal QQ Plot of Residuals\n(with Highlighted Outlier)")
qqline(residuals, col = "darkorange", lwd = 2)

# Highlight the outlier’s residual on the QQ‐plot
#  a) Find the theoretical quantile for the 10th largest order statistic
ord <- order(residuals)
ranks <- rank(residuals, ties.method = "first")
theor_q <- qnorm((ranks[10] - 0.5) / n)

points(theor_q, residuals[10], col = "blue", pch = 19, cex = 1.3)
text(theor_q, residuals[10] + 1, labels = "Outlier", col = "blue", cex = 0.8, pos = 3)

# 1) Simulate linear data with constant variance, then add one influential point
set.seed(123)
n  <- 50
x  <- runif(n, 0, 10)
y  <- 1 + 2 * x + rnorm(n, sd = 1)  # true model: y = 1 + 2x + noise

# Add the influential observation
x_inf <- 12
y_inf <- -5
x_all <- c(x, x_inf)
y_all <- c(y, y_inf)

# 2) Fit two linear models:
#    (a) model_all on (x_all, y_all)  – includes the influential point
#    (b) model_base on (x, y)         – excludes the influential point
model_all  <- lm(y_all ~ x_all)
model_base <- lm(y ~ x)

# 3) Create a grid of x-values covering the range of x_all
x_grid <- seq(min(x_all) - 0.5, max(x_all) + 0.5, length.out = 200)

# 4) Compute the predicted y for both models on that grid
pred_all  <- predict(model_all,  newdata = data.frame(x_all = x_grid))
pred_base <- predict(model_base, newdata = data.frame(x = x_grid))

# 5) Plot all data points and overlay both regression lines
plot(x_all, y_all,
     pch    = 19,
     col    = "darkgreen",
     xlab   = "Predictor (x)",
     ylab   = "Response (y)",
     main   = "Regression Lines: With (solid) vs. Without (dashed) Influential Point")
points(x_inf, y_inf, col = "red", pch = 19, cex = 1.5)     # highlight the influential point
text(x_inf, y_inf, "Influential Point", col = "red", pos = 2, cex = 0.8)

# 6) Add the “with influential” fit as a solid black line
lines(x_grid, pred_all,
      col    = "black",
      lwd    = 2,
      lty    = 1)     # solid

# 7) Add the “without influential” fit as a dashed dark blue line
lines(x_grid, pred_base,
      col    = "darkblue",
      lwd    = 2,
      lty    = 2)     # dashed

# 8) Add a legend
legend("topleft",
       legend = c("Fit w/ Influential Point", "Fit w/o Influential Point ", "Data Points", "Influential Point"),
       col    = c("black", "darkblue", "darkgreen", "red"),
       pch    = c(NA, NA, 19, 19),
       lty    = c(1, 2, NA, NA),
       lwd    = c(2, 2, NA, NA),
       pt.cex = c(NA, NA, 1, 1.5),
       bty    = "n")


#Worked Example **Change read
insurance <- read.csv("C:/Users/jenn9/Downloads/insurance(1).csv")
plot(insurance$age, insurance$charges)
reg1=lm(insurance$charges~insurance$age)
summary(reg1)
# Plot residuals vs. fitted values
plot(fitted(reg1), resid(reg1),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)

plot(insurance$bmi, insurance$charges)
reg1=lm(insurance$charges~insurance$bmi)
summary(reg1)
# Plot residuals vs. fitted values
plot(fitted(reg1), resid(reg1),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)

#See all diagnostic plots
par(mfrow = c(2, 2))
plot(reg1)
par(mfrow = c(1, 1))  # Reset to default

shapiro.test(resid(reg1))

#Worked Example using Pulizer Prize data 
# For details see:  https://fivethirtyeight.com/features/do-pulitzers-help-newspapers-keep-readers/ 
library(ggplot2)
pulitzer <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP311/master/DataSets/pulitzer.csv")
head(pulitzer)

#Using base R 
plot(pulitzer$num_finals1990_2014, pulitzer$pctchg_circ, 
     xlab="Number of Pulitzer Prize winners from 1990 to 2014", 
     ylab="percent change in daily newspaper circulationfrom 2004 to 2013")
reg1=lm(pulitzer$pctchg_circ~pulitzer$num_finals1990_2014)
abline(reg1)

#Plot using ggplot2 
#Requires ggplot2 library - remove # in  next line if needed 
# library(ggplot2)  
ggplot(pulitzer, aes(x = num_finals1990_2014, y = pctchg_circ)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color ="blue") +# Adding a regression line
  labs(
    title = "Relationship between Circulation Rates and Number of 
    Pulitzer Prize Winning Journalists",
    x = "Number of Pulitzer Prize Winners from 1990-2014",
    y = "Percent change in Daily Circulation from 2004 to 2013",
    caption = "Data from fivethirtyeight package in R"
  ) +
  theme_minimal()

#Fit and output parameter estimates for the least squares model
reg1=lm(pctchg_circ~num_finals1990_2014,data=pulitzer)
summary(reg1)

# Create a new data frame for prediction
new_data <- data.frame(num_finals1990_2014 = c(25, 75))

# Predict using the model
predicted_values <- predict(reg1, newdata = new_data)

# Print the predicted values
predicted_values

# Correlation matrix 
cor(pulitzer[,c('pctchg_circ','num_finals1990_2014')])


