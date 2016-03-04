library(dplyr)
library(tidyr)

set.seed(100) # for reproducibility
action <- sample(letters[1:5], 100, replace = TRUE) # dummy up sales actions (independent variable)
converted <- as.logical(rbinom(100, 1, prob = .5))  # dummy up physician conversion (dependent variable)

(df <- data.frame(action = action, converted = converted))

# Compute conversion rates for each action
conv_df <- df %>% 
  group_by(action, converted) %>% # unique combinations of action by converted
  summarize(freq = n()) %>% # compute freq of unique combinations
  spread(converted, freq) %>% # spread rows to cols
  mutate(rate = `TRUE`/sum(`TRUE`,`FALSE`)) # compute conversion rate

conv_df[order(-conv_df$rate),] #reverse sort
