# SSM ~ itsblerdlines

####
# Basic Exploration and Calculations using COVID-19 data to
# determine if age and gender have been significant factors
# in COVID-19 deaths
####

# Remove previously stored variables
rm(list=ls()) 

# Install packages and libraries
install.packages('Hmisc')
library(Hmisc) 

# Import data
c_19 <- read.csv("~COVID19_data.csv")

# Look at data
summary(c_19)
str(c_19)

# Clean the 'death' column
c_19$death_dummy <- as.integer(c_19$death != 0)

# Calculate % death rate
sum(c_19$death_dummy) / nrow(c_19)

# Is age a significant factor in COVID deaths

deceased = subset(c_19, death_dummy == 1)
living = subset(c_19, death_dummy == 0)
mean(deceased$age, na.rm = TRUE) #68.58621
mean(living$age, na.rm = TRUE) #48.07229
# is this statistically significant?
t.test(living$age, deceased$age, alternative="two.sided", conf.level = 0.99)

# The p-value is < 2.2e-16, so we reject the null hypothesis and 
# conclude that age is statistically significant in COVID deaths

# Is gender a significant factor in COVID deaths
male = subset(c_19, gender == "male")
female = subset(c_19, gender == "female")
mean(male$death_dummy, na.rm = TRUE) #8.5%!
mean(female$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(male$death_dummy, female$death_dummy, alternative="two.sided", conf.level = 0.99)

# Using 99% confidence in the t test males have an 8.8% higher chance of dying.
# The p-value = 0.002105 is < 0.05 so gender is a statistically significant factor
# in COVID deaths
