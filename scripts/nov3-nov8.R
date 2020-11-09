library(tidyverse)
library(modelr)

affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv") %>% 
  mutate(cheater = affairs > 0)

# binary logistic regression predicting 
# prob of an affair as a function of yearsmarried
M <- glm(cheater ~ yearsmarried, 
         data = affairs_df, 
         family = binomial(link = 'logit'))

summary(M)

# Log odds or logit transformation

theta <- 0.6
phi <- log(theta/(1-theta))

# theta is a probability
# theta/(1 - theta) is a "odds"


# What is the log odds that someone has an affair
# if yearsmarried = 10?

# according to the model
# the log odds is a linear function of yearsmarried
# the coefficients of the linear function are...
coef(M)

betas <- coef(M)
# so therefore, the log odds of having an affair if yearsmarried is 10
# is as follows:
w <- betas[1] + betas[2] * 10

# ok, that's a log odds, but what's the probability?
# well, we do inverse_logit( betas[1] + betas[2] * 10)
# we can do the inverse logit with plogis
plogis(betas[1] + betas[2] * 10)

inverse_logit <- plogis
inverse_logit(betas[1] + betas[2] * 10)

# What is the log odds that someone has an affair
# if `yearsmarried` = 20?
betas[1] + betas[2] * 20

# ok, what's the probability ?
inverse_logit(betas[1] + betas[2] * 20)

# predicted log odds
data.frame(yearsmarried = c(1, 5, 10, 15, 20, 25)) %>% 
  add_predictions(M)

# predicted probabilities
data.frame(yearsmarried = c(1, 5, 10, 15, 20, 25)) %>% 
  add_predictions(M, type = 'response')
