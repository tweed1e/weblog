
library(rvest)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(glmnet)

url <- "http://www.imdb.com/title/tt0108778/episodes?season="

urls <- map_chr(1:10, function(n) {paste0(url, n)})

pages <- map(urls, read_html)


titles <- pages %>% 
  map(html_nodes, css = 'strong a') %>% 
  map(html_text) %>% 
  enframe() %>% # wow, this is a long pipe.
  unnest() %>%  # is enframe() %>% unnest() the best way?
  rename(season = name, title = value) %>% 
  filter(grepl("^The One", title)) %>% 
  group_by(season) %>% 
  mutate(episode = row_number()) %>% 
  select(season, episode, title)
titles

stars <- pages %>% 
  map(html_nodes, css = '.ipl-rating-star__rating')%>% 
  map(html_text) %>% 
  enframe() %>%
  unnest() %>% 
  group_by(name) %>% 
  filter(row_number() %in% seq.int(1, max(row_number()), by = 23))

stars <- stars %>% 
  mutate(episode = row_number(), value = as.numeric(value)) %>%
  select(season = name, episode, rating = value)

titles <- inner_join(titles, stars) # yea, great.

# rating distribution
# ggplot(titles, aes(x = rating)) + geom_histogram()

# include 10 rating on plot.somtimes have to say plot.new(), not sure if it'll work in .Rmd
ggplot(titles, aes(x = factor(season), y = rating, group = season)) + 
  geom_boxplot() + ylim(NA, 10) +
  labs(title = "Friends Ratings By Season", x = "Season #", y = "Rating")

# alright, so Seasons 4-6 look the best. Season 1 was just getting going, and Season 9 is trash (I'll blame Ross).
# What are the outliers? In S4 and S6-S9, one episode per season is among the worst of the series.
# Why? 

# Check titles of the best and worst episodes by season? Maybe that will give some clues

titles %>% group_by(season) %>% filter(rating == min(rating)) %>% ungroup() %>% arrange(rating)
# The worst episodes come at the end of the seasons in 4,5,6,7,8,9,1. Hmm.
# say something funny about them. C.H.E.E.S.E. come on.
# Invitation is a Ross/Rachel clip-show (laaaame)


titles %>% group_by(season) %>% filter(rating == max(rating)) %>% ungroup() %>% arrange(-rating)
# Good episodes: love related, middle of season or season-finale.
# Pass the smell test?
# The One with the Embryos---so good. Prom Video---good. 
# Everybody finds outs---great ep (other than the Phoebe line I always hated
# "my eyes! my eyes!"). A great example of common knowledge and 
# rational agents in game theory.

# But other problem: the title "The One with the Embryos" gives no information
# other than a noun that is related to babies. Frig. No character names, no indication
# of why I like it.

# classic omitted variable bias that economists love and hate:
# an individual executive or team decides where to put the episode 
# based on unobserved (to us, but not them) episode quality. 
# unobserved episode quality also affects the
# observed rating (through some consumer utility process)

# anyway, at this point, just think about it / explain it.

t <- titles %>% 
  mutate(title_ = gsub("The One ", "", title)) %>% 
  unnest_tokens(word, title_) %>% 
  anti_join(stop_words) %>%
  mutate(word = gsub("'s", "", word))
# t %>% select(season, episode, word) 
# group_by(season, episode, title) #%>% 
  # summarize(title_ = paste(word, collapse = " ")) 
w <- t %>% ungroup() %>% select(season, episode, rating, word) %>% mutate(x = 1) %>% spread(key = word, value = x, fill = 0)
w <- w %>% 
  mutate_at(vars(-rating), factor) %>% 
  select(rating, season, episode, 
         chandler, joey, monica, phoebe, rachel, ross, 
         everything()) %>% 
  rename(v_1 = `1`, v_2 = `2`, v_break = `break`)
# ok cool that was easy

# ok cool, cool cool.
x <- sparse.model.matrix(~ ., w[,-1])[,-1]

# fit <- glmnet(x, w %>% pull(rating))

cvfit <- cv.glmnet(x, w %>% pull(rating))
plot(cvfit)
c <- coef(cvfit, s = "lambda.min")

dimnames(c)
coefs <- c %>% as.matrix() %>% as_tibble()
coefs <- coefs %>% mutate(var = dimnames(c)[[1]]) %>% select(var, coef = `1`)

# so if coef non-zero, put that in OLS estimation.
# how many non-zero? need less than 224 or whatever.
test <- coefs %>% filter(!near(coef, 0)) # 124 vars. mean is 8.47 = pretty good show overall.

# then just run lm but only on the sparse.model.matrix
# check x
e <- x[, dimnames(x)[[2]] %in% (test[-1, ] %>% pull(var))] #%>% dim()
dim(x) # ok cool. now use lm on that.



# put rating and x... etc together.
new <- bind_cols(w[, "rating"], e %>% as.matrix() %>% as_tibble())
lm(rating ~ ., data = new) %>% summary()
## error. maybe have to rename columns. 1 -> one, 2 -> two

# some are NA--why? consider mac and cheese. only one episode with both,
# which means `mac` and `cheese` are perfectly collinear (linear combinations of each other)

## saev them, then maybe plot coefs vs s.e. or p-value, and label them using gglabel or w/e.
# broom it!
cdf <- lm(rating ~ ., data = new) %>% tidy() # dropped NA values, hmm.
cdf[-1, ] %>% as_tibble() %>% arrange(-abs(estimate)) %>% head(15)
# names(w)
# notice: no names. e.g., "The One Where Rachel..."
cdf[-1, ] %>% as_tibble() %>% arrange(-estimate) %>% head(15)
# partly bc these are just outliers, coeffs that identify single episodes
# bc rachel, ross etc are in more, sort by std.error to see what's up with that.
# std.error basically a function of the variance of the predictor
cdf[-1, ] %>% as_tibble() %>% arrange(std.error) %>% filter(p.value < 0.01) %>% head(15)
# ross has positive effect
cdf[-1, ] %>% as_tibble() %>% arrange(p.value) %>% filter(p.value < 0.01 & std.error < 0.1)
# ross! which is funy bc ross is such an absolute trash heap

# what are ross's epsiodes? or boxplot by character?
# titles %>% filter(grepl("Ross", title))
# titles %>% filter(grepl("Rachel", title))
# titles %>% filter(grepl("Chandler", title))

# what to draw from this? can we just use seasons and episodes
# then how much do characters add?
# sparse.model.matrix(~ ., )[,-1]
lm(rating ~ season + episode, data = w) %>% summary()
# only 25%! ok.
lm(rating ~ season + episode + chandler + joey + monica + phoebe + ross + rachel, data = w) %>% summary()
# hmm. 28%. And slightly different conclusions from the full model.
clm <- lm(rating ~ season + episode + chandler + joey + monica + phoebe + ross + rachel, data = w) %>% tidy()
inner_join(cdf, clm, by = c("term")) %>% select(term, estimate.x, estimate.y)
# now use that to create model.matrix or whatever. convert to factors, including seasons and episodes.
# wow. alright. frig. try to force glmnet to give us non-zero coefficients for all the seasons, episodes, and characters.


# nvars;
# penalty = 0 for the constant 
# number of seasons +
# number of different episodes + 
# 6
n_keep_vars <- max(titles$season) + max(titles$episode) + 6
penalty.factor <- c(rep(0, n_keep_vars), rep(1, ncol(x) - n_keep_vars))


# x1 <- sparse.model.matrix(~ ., w[,-1])[,-1]
# fit <- glmnet(x, w %>% pull(rating))

cvfit1 <- cv.glmnet(x, w %>% pull(rating), penalty.factor = penalty.factor)
plot(cvfit1)
c1 <- coef(cvfit1, s = "lambda.min")

dimnames(c1)
coefs1 <- c1 %>% as.matrix() %>% as_tibble()
coefs1 <- coefs1 %>% mutate(var = dimnames(c1)[[1]]) %>% select(var, coef = `1`)

# inner_join(coefs1[-1, ], clm[-1, ], by = c("var" = "term")) %>% filter(!grepl("season|episode", var)) %>%  ggplot(aes(x = coef, y = estimate)) + geom_point()
inner_join(coefs1[-1, ], clm[-1, ], by = c("var" = "term")) %>%  ggplot(aes(x = coef, y = estimate)) + geom_point()
# name S, E, remove the from names1, then gglabel

# not the same, but close.
# anyway, let's focus on the post-selection lm.
cdf %>% as_tibble() %>% arrange(estimate) %>% filter(p.value < 0.01 & std.error < 0.1)
cdf[-1, ] %>% as_tibble() %>% arrange(-estimate) %>% filter(std.error < 0.1)

# cool: vegas great.
# finales also great.
# ross (ugh)
# weddings, season

# then just do post-selection estimator

