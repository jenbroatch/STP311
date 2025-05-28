#Module 1
#Simple Linear Regression 
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


