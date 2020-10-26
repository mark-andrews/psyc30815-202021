library(tidyverse)

ansur_df <- read_csv("http://data.ntupsychology.net/ansur.csv")

# Make a histogram of weight by height tercile and age tercile
p <- ggplot(ansur_df, 
            aes(x = weight)
) + geom_histogram(binwidth = 5, colour = 'white') +
  facet_grid(age_tercile ~ height_tercile)


# weight predicted from height
model_1 <- lm(weight ~ height, data = ansur_df)

# let's look at the coefficients
coef(model_1)

# weight predicted from height and age
model_2 <- lm(weight ~ height + age, data = ansur_df)

