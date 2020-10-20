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
ggplot(ansur_df, 
       aes(x = height, y = weight, colour = gender)
) + geom_point(size = 0.5)


# Make a histogram of weight by gender
ggplot(ansur_df, 
       aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 5, colour = 'white')
