# do the post-selection inference!

library(werfriends)
library(tidyverse)

library(tidytext)
library(ggplot2)
library(ggrepel)
library(glmnet)

# post-selection inference
# find link to paper, etc.
#
# idea: what makes a good title?
# more specific q: what is effect of including each character in title on ep rating?

# what controls? emphasize diff in prediction and 
# season #, ep #, characters, words; p>>n.
# idea: post-selection inference.
# regress all character `treatments` on all explanatory vars
# regress rating on all explanatory vars (but not characters)
# include all selected variables in resulting lm.
# want to focus on inference, not prediction

titles <- werfriends::friends_episodes %>% as_tibble()

titles

writ <- titles %>% select(season, episode, word = writers) %>% unnest(word) %>% 
  mutate(word = gsub('\\s', '', word) %>% tolower(), x = 1) %>% 
  spread(key = word, value = x, fill = 0) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_all(factor)

# and same for directors.
dirs <- titles %>% select(season, episode, word = director) %>% unnest(word) %>% 
  mutate(word = gsub('\\s', '', word) %>% tolower(), x = 1) %>% 
  spread(key = word, value = x, fill = 0) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_all(factor)
dirs


w <- titles %>% select(-director, -writers) %>%  
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>% 
  mutate(word = gsub("'s", "", word), x = 1) %>% # replace Joey's with Joey, etc.
  spread(key = word, value = x, fill = 0) %>% 
  rename(v_1 = `1`, v_2 = `2`, v_break = `break`) %>% 
  select(rating, chandler, joey, monica, phoebe, rachel, ross, 
         season, episode, everything()) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_at(vars(-rating), factor)
# ^ sparse.model.matrix wasn't happy with some column names break glmnet
w
lincomb <- caret::findLinearCombos(w)
w <- w[, -lincomb$remove] # ok cool. put that in somewhere before.
# ?findLinearCombos
w


