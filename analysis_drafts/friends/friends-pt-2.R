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
  mutate(word = gsub('\\s|[[:punct:]]', '', word) %>% tolower(), x = 1) %>% 
  spread(key = word, value = x, fill = 0) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_all(factor)

# and same for directors.
dirs <- titles %>% select(season, episode, word = director) %>% unnest(word) %>% 
  mutate(word = gsub('\\s|[[:punct:]]', '', word) %>% tolower(), x = 1) %>% 
  spread(key = word, value = x, fill = 0) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_all(factor)
dirs

# need to keep season, episode so I can 

wo <- titles %>% select(-director, -writers) %>%  
  unnest_tokens(word, title) %>% 
  anti_join(stop_words %>% filter(!word %in% c("where","with","after","everybody"))) %>% # except some titles are all stop words
  mutate(word = gsub("'s", "", word), 
         word = ifelse(grepl("1|2|break", word), paste0("v_", word), word), 
         x = 1) %>% # replace Joey's with Joey, etc.
  spread(key = word, value = x, fill = 0) %>% 
  # rename(v_1 = `1`, v_2 = `2`, v_break = `break`) %>% 
  select(rating, chandler, joey, monica, phoebe, rachel, ross, 
         season, episode, everything()) %>% 
  ungroup() %>% # need the ungroup here before the factor call. ugh.
  mutate_at(vars(-rating), factor)
# ^ sparse.model.matrix wasn't happy with some column names break glmnet
# library(tidyeval)
# w %>% rename_("v_1" = `1`) #, v_2 = `2`, v_break = `break`)
# xxx

# cool
episodes <- full_join(wo, dirs) %>% full_join(writ)
# dim(episodes)
episodes %>% glimpse()
# dsadsa

# anti_join(dirs, wo) %>% select(season, episode) %>% mutate_all(as.numeric) %>% left_join(titles)
# two episodes missing from the ughhhh.

ep_mm <- sparse.model.matrix(~ ., episodes %>% select(-rating, -n_ratings))[,-1] 
# oops, matrix
lincomb <- caret::findLinearCombos(ep_mm)
ep_mm <- ep_mm[, -lincomb$remove] # ok cool. put that in somewhere before.
dim(ep_mm)
# ?findLinearCombos
# x
# x %>% dim()

# ok cool, now we ready to do post-selection inf.
# what to write: want to investigate characters
# so do lm on characters and seasons/eps
# but omitted var bias---control for writer / director / other title words
# but how to pick.
# use lasso to pick directly (with no penalty for ones we interested in)---
# but that's for prediction; lasso will purposely drop
# vars that are highly correlated with the characters
# that'll make for robust rating prediction, but I want the
# effect of the character name itself.
# (e.g., the diff between "predict the rating based on this title name, S, E, D, W combo?" and
# "what rating bump will I get if I include 'Rachel' in the ep title?")

# diff ways:
# lm on S/E/character
# naive <- lm(rating ~ season + episode + chandler + joey + monica + phoebe + rachel + ross, data = w)
naive <- lm(rating ~ season + episode + chandler + joey + monica + phoebe + rachel + ross, data = episodes)
# all relative to the pilot.
# ok, Joey sucks, Phoebe sucks, Ross is great (alarm bells should be going off bc Ross is trash)
naive %>% tidy() %>% tail() #%>% as_tibble() #%>% filter(p.value < 0.1) %>% arrange(-abs(estimate))
# what to learn: Joey bad!


# LASSO on everything; no penalty on characters
# n_keep_vars <- max(titles$season) + max(titles$episode) + 6
# penalty.factor <- c(rep(0, n_keep_vars), rep(1, ncol(x) - n_keep_vars))
penalty.factor <- c(rep(0, 6), rep(1, ncol(ep_mm) - 6))
c <- cv.glmnet(ep_mm, episodes$rating, penalty.factor = penalty.factor) %>%
  coef(s = 'lambda.min') %>% .[-1,]

lasso <- tibble(term = names(c), estimate = c) # that's easier.
lasso

# hmm, weird. and also weird if you do LASSO without penalty.
# so first one doesn't control for anything but S/E
# second one controls for things that aren't as correlated with the
# vars we interested in
# but that gives biased estimates character effects. Ross can't be the important.

# write function that does glmnet, returns names of non-zero coefficients
get_lasso_coefs <- function(name, x) { # or name (`chr`) of column.
  # input: y; 
  c <- cv.glmnet(x[, colnames(x) != name], x[,name]) %>%
    coef(s = 'lambda.min') %>% .[-1,]
  names(c)[c != 0]
}

get_lasso_coefs_ <- function(y, names, x) { # or name (`chr`) of column.
  # input: y; 
  c <- cv.glmnet(x[, !colnames(x) %in% names], y) %>%
    coef(s = 'lambda.min') %>% .[-1,]
  names(c)[c != 0]
}

# y <- episodes$rating
# c <- cv.glmnet(x[, !colnames(x) %in% names], y) %>%
#   coef(s = 'lambda.min') %>% .[-1,]
# names(c)[!near(c,0)]

# list of characters
friends <- c("chandler", 
             "joey", 
             "monica", 
             "phoebe", 
             "rachel", 
             "ross") %>% 
  map_chr(paste0, "1")

# map list of characters into lasso coefficient function
lasso_coefs <- friends %>%
  map(get_lasso_coefs, x = ep_mm) %>% 
  unlist() 

# process the variables
# post_vars <- c(lasso_coefs, get_lasso_coefs_(episodes$rating, friends, ep_mm)) %>% sort() 
post_vars <- c(friends, 
               lasso_coefs,
               get_lasso_coefs_(episodes$rating, friends, ep_mm)) %>% unique()

# need to spread season data and only pick ones 
post_vars <- post_vars %>% 
  as_tibble() %>% 
  mutate(value = ifelse(grepl("season|episode", value), 
                        value, 
                        gsub("\\d$", "", value))) %>%
  pull(value) %>% c(gsub("1", "", friends), .) %>% unique()

post_dat <- episodes %>% mutate(x = 1) %>% 
  spread(key = season, value = x, fill = 0, sep = "") %>% mutate(x = 1) %>% 
  spread(key = episode, value = x, fill = 0, sep = "") %>% 
  mutate_at(vars(-rating), factor) %>% 
  select(one_of(c("rating", post_vars)))

f <- paste("rating ~ ", paste(post_vars, collapse = " + "))

# coefs.
lm(f, data = post_dat) %>% summary() %>% tidy() %>% .[2:7,]
# nothing significant!
# no effects of names!

# ok, collect these and geom_barchart
# geom_bar(), group = coef?, colour = model?
# put all coefs together

cof <- bind_rows(
  lm(f, data = post_dat) %>% summary() %>% tidy() %>% .[2:7,] %>% as_tibble() %>% mutate(model = "post"),
  naive %>% tidy() %>% tail() %>% as_tibble() %>% mutate(model = "lm"),
  lasso[1:6, ] %>%  mutate(model = "lasso")
)

cof %>% 
  ggplot(aes(x = factor(term), y = estimate, group = factor(model), fill = factor(model))) + 
  geom_col(position = 'dodge') + 
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    width = 0.4,
    position = position_dodge(width = 0.9)
  )

# error_bars?
# nothing significant in post-selection.
# in almost every case, the post-selection estimates are lower than lasso. 
# simple lm closest to 0, but mistakenly thinks Joey tanks every episode


# post-selection inference; LASSO rating, x; LASSO {each character}, x; 
# then lm rating on U{all non-zero coefficients from all LASSOs}
# then compare character estimates from each regression
# what bias look like?
# what final conclusion?


