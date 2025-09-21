#Module 3 
#Multiple Linear Regression \


library(Lock5Data)
View(SleepStudy) 
reg1=lm(SleepStudy$DepressionScore~SleepStudy$PoorSleepQuality )

plot(SleepStudy$PoorSleepQuality , SleepStudy$DepressionScore, xlab="Measure of sleep quality (higher values are poorer sleep)", ylab="Measure of degree of depression")
abline(reg1)
summary(reg1)
plot(reg1)

reg2=lm(SleepStudy$DepressionScore~SleepStudy$PoorSleepQuality+SleepStudy$Stress )
summary(reg2)


ggplot(SleepStudy, aes(x=Stress, y=DepressionScore)) + geom_boxplot() +
  ggtitle("Distribution Depression Score by Stress Level")+
  xlab("Stress Level") 

#R Version of SAS Learning Lab
# See https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/.

# Load packages
install.packages('fivethirtyeightdata', repos =
                   'https://fivethirtyeightdata.github.io/drat/', type = 'source')
library(fivethirtyeight)
# Load dataset
data("college_recent_grads")

# Check structure
View(college_recent_grads)

# Define STEM major categories
stem_categories <- c("Engineering", "Biology & Life Science", 
                     "Computers & Mathematics", "Physical Sciences", 
                     "Health", "Agriculture & Natural Resources")

college_recent_grads <- college_recent_grads %>%
  mutate(stem = ifelse(major_category %in% stem_categories, 1, 0))
# sharewomen is a proportion:  0 to 1
# However, that makes the "unit" of coefficient represent change from 0 to 1 
# Let's change the value to a % so that a 1 unit change is 1% 
college_recent_grads <- college_recent_grads %>%
  mutate(sharewomen_percent = sharewomen * 100)

write.csv(college_recent_grads, "C:/Users/jenn9/ASU Dropbox/Jennifer Broatch/Courses/A_STP311Online/collegemajor538.csv")

# Fit simple  linear regression model
model <- lm(median ~ sharewomen_percent , 
            data = college_recent_grads)

# Show model summary
summary(model)

# Fit multiple linear regression model
model <- lm(median ~ unemployment_rate + sharewomen_percent + stem , 
            data = college_recent_grads)

# Show model summary
summary(model)