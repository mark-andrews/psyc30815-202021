library(tidyverse)

affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv") %>% 
  mutate(cheater = affairs > 0)

# binary logistic regression predicting 
# prob of an affair as a function of yearsmarried
M <- glm(cheater ~ yearsmarried, 
         data = affairs_df, 
         family = binomial(link = 'logit'))

summary(M)
