library(tidyverse)

ansur_df <- read_csv("http://data.ntupsychology.net/ansur.csv")

# weight predicted from height and age
model <- lm(weight ~ height + age, data = ansur_df)

# get summary of the model
summary(model)

# pull out the coefficients table
summary(model)$coefficients

# confidence intervals for the coefficients
confint(model)

# 50% confidence intervals 
confint(model, level = 0.5)
# 75% confidence intervals 
confint(model, level = 0.75)
# 90% confidence intervals 
confint(model, level = 0.9)
# 99% confidence intervals 
confint(model, level = 0.99)

# R^2
summary(model)$r.sq


# adjusted R^2
summary(model)$adj.r.sq


