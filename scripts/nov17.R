library(tidyverse)
library(modelr)

# load data ---------------------------------------------------------------------

affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv")

M <- glm(affairs ~ yearsmarried,
         family = poisson(link = 'log'),
         data = affairs_df)

summary(M)         

intercept <- coef(M)[1]
slope <- coef(M)[2]

# What is the average number of affairs by a person
# with 10 years of marriage
# What is the predicted mean of the Poisson distribution over affairs
# for someone with 10 years of marriage.
log_lambda <- intercept + slope * 10
lambda <- exp(log_lambda)

# What is the predicted mean of the Poisson distribution over affairs
# for someone with 11 years of marriage.
log_lambda <- intercept + slope * 11
lambda <- exp(log_lambda)

# Get predicted means of Poisson distribution for 
# range of values of yearsmarried
tibble(yearsmarried = seq(1, 20)) %>% 
  add_predictions(M, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred)) + geom_line() + geom_point()

M2 <- glm(affairs ~ yearsmarried + age + gender,
          family = poisson(link = 'log'),
          data = affairs_df)

anova(M, M2, test = 'Chisq')
anova(M2, M, test = 'Chisq')

# Looking at the meaning of coefficients in M2
exp(coef(M2)[2:4])

tibble(yearsmarried = seq(1, 20),
       age = median(affairs_df$age),
       gender = 'female') %>% 
  add_predictions(M2, type = 'response')  %>% 
  ggplot(aes(x = yearsmarried, y = pred)) + geom_line() + geom_point()

