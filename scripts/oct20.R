library(tidyverse)

ansur_df <- read_csv("http://data.ntupsychology.net/ansur.csv")

# Make a scatterplot of weight by height
ggplot(ansur_df, 
            aes(x = height, y = weight)
) + geom_point(size = 0.5, alpha = 0.5)

# Make a histogram of weight
ggplot(ansur_df, 
       aes(x = weight)
) + geom_histogram(binwidth = 5, colour = 'white')

# Make a scatterplot of weight by height
# with colour coding of gender
ggplot(mapping = aes(x = height, y = weight, colour = gender),
       data = ansur_df) + geom_point(size = 0.5)


# Make a histogram of weight by gender
ggplot(ansur_df, 
       aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 5, colour = 'white')


# an example custom function
f <- function(x, y) {x/y}


# Make a histogram of weight by height tercile
ggplot(ansur_df, 
       aes(x = weight)
) + geom_histogram(binwidth = 5, colour = 'white') +
  facet_wrap(~height_tercile)

# calculate mean and sd of weight 
summarize(ansur_df, 
          average = mean(weight),
          st_dev = sd(weight))


# calculate mean and sd of weight 
# for each height tercile group
summarize(group_by(ansur_df, height_tercile), 
          average = mean(weight),
          st_dev = sd(weight))

# calculate mean and sd of weight 
# for each height tercile group (using the %>% )
group_by(ansur_df, height_tercile) %>% 
  summarize(average = mean(weight),
            st_dev = sd(weight))


# calculate mean and sd of weight, and mean of height,
# for each height tercile group
summarize(group_by(ansur_df, height_tercile), 
          average = mean(weight),
          st_dev = sd(weight),
          avg_height = mean(height))
