library(tidyverse)
library(modelr)

ansur_df <- read_csv("http://data.ntupsychology.net/ansur.csv")

# weight predicted from height and age
model <- lm(weight ~ height + age, data = ansur_df)

# Meaning of coefficients

# Prediction in lm using `predict`
ansur_df_new <- tibble(age = 25,
                       height = seq(140, 200))

predict(model, newdata = ansur_df_new)

ansur_df_new_pred <- add_predictions(ansur_df_new, model)

ggplot(ansur_df_new_pred,
       aes(x = height, y = pred)
) + geom_line()

## prediction confidence intervals
confint(model)

predict(model, 
        newdata = ansur_df_new,
        interval = 'confidence')

# Nested model comparison
model_ha <- lm(weight ~ height + age, data = ansur_df)
model_h <- lm(weight ~ height, data = ansur_df)
model_a <- lm(weight ~ age, data = ansur_df)
model_null <- lm(weight ~ 1, data = ansur_df)

anova(model_h, model_ha)
anova(model_a, model_ha)
anova(model_null, model_a)

# Using binary predictor variables
model_hg <- lm(weight ~ height + gender, data = ansur_df)

expand_grid(height = seq(140, 200),
            gender = c('female', 'male')) %>% 
  add_predictions(model_hg) %>% 
  ggplot(aes(x = height, y = pred, colour = gender)) +
  geom_line()

# looking at race
ansur_df2 <- filter(ansur_df,
       race %in% c('white', 'black', 'hispanic')
)
# Using binary predictor variables
model_hr <- lm(weight ~ height + race, data = ansur_df2)
summary(model_hr)

expand_grid(height = seq(140, 200),
            race = c('white', 'black', 'hispanic')) %>% 
  add_predictions(model_hr) %>% 
  ggplot(aes(x = height, y = pred, colour = race)) +
  geom_line()

model_h <- lm(weight ~ height, data = ansur_df2)
anova(model_h, model_hr)


# varying slope and varying intercept model

# vi
model_hg <- lm(weight ~ height + gender, data = ansur_df)

# vi and vs 
model_hg_i <- lm(weight ~ height * gender, data = ansur_df)

expand_grid(height = seq(140, 200),
            gender = c('female', 'male')) %>% 
  add_predictions(model_hg_i) %>% 
  ggplot(aes(x = height, y = pred, colour = gender)) +
  geom_line()


anova(model_hg, model_hg_i)
