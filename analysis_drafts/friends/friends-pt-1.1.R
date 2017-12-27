
library(rvest)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggrepel)
library(glmnet)

# idea: what makes a good title?
# more specific q: what is effect of including each character in title on ep rating?
# what controls? 
# season #, ep #, characters, words; p>>n.
# idea: post-selection inference.
# regress all character `treatments` on all explanatory vars
# regress rating on all explanatory vars (but not characters)
# include all selected variables in resulting lm.
# want to focus on inference, not prediction

url <- "http://www.imdb.com/title/tt0108778/episodes?season="

urls <- map_chr(1:10, function(n) {paste0(url, n)})

pages <- map(urls, read_html)

x <- read_html(paste0(url, "1"))
x # get list of eps, and follow links.
s <- html_session(paste0(url, "1"))

# dang, purrr::keep to the rescue, colin fay right.
s %>% jump_to(url = 
x %>% html_nodes(css = 'strong a') %>% html_attr("href") %>% keep(grepl, pattern = "title") %>% 
  .[1]) %>% read_html() %>% html_nodes("div h1[itemprop=\"name\"]") %>% html_text()
# dang, itemprop to the rescue.
# so good.

s %>% jump_to(url = 
                x %>% html_nodes(css = 'strong a') %>% html_attr("href") %>% keep(grepl, pattern = "title") %>% 
                .[1]) %>% read_html() %>% html_nodes('span[itemprop="ratingValue"]') %>% html_text()

s %>% jump_to(url = 
                x %>% html_nodes(css = 'strong a') %>% html_attr("href") %>% keep(grepl, pattern = "title") %>% 
                .[1]) %>% read_html() %>% html_nodes('span[itemprop="ratingCount"]') %>% html_text()

# now follow and get director, writers
# get link that says "See full cast & crew"
qq <- s %>% jump_to(url = 
                x %>% html_nodes(css = 'strong a') %>% html_attr("href") %>% keep(grepl, pattern = "title") %>% 
                .[1]) 

# writers:
qq %>% 
  follow_link(i = "See full cast & crew") %>%
  html_nodes(".simpleCreditsTable") %>% 
  html_table() %>% .[[2]] %>% # I think the writers are in the second table in the list of tables?
  filter(grepl("written by", X3)) %>%  .[,1]#X3 is the job.
#21 of those...

# ok, then what.

# x <- rerun(5, a = rbernoulli(1), b = sample(10))
# x
# x %>% keep("a")
# select titles.

# first thing, I'll just get writers, directors, etc and give stats.
# try to do the rating better, since that rule might go wrong if they
# change the site.

xxx

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

# [link to SO post here](https://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r)
is_outlier <- function(x) {
  x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)
}

# include 10 rating on plot.somtimes have to say plot.new(), not sure if it'll work in .Rmd
titles %>% 
  # mutate(outlier = ifelse(is_outlier(rating), paste0("S", season, "E", episode), NA)) %>% 
  mutate(outlier = ifelse(is_outlier(rating), title, NA)) %>% 
  ggplot(aes(x = factor(season), y = rating, group = season)) + 
    geom_boxplot() + ylim(NA, 10) +
    geom_text_repel(aes(season, rating, label = outlier)) +
    labs(title = "Friends Ratings By Season", x = "Season #", y = "Rating")

# what other things?



xxx


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

# need a model.
# w # spread.
# x # sparse.model.matrix

# three models.
# 1. lm with season/ep/character
# 2. glmnet, but want to compare with other model.
# 2. 0 penalty for s/e/c vars
# compare those.
# 3. then post-selection lm; R^2^ compared to other, coefficients compared to other.
# what's important?

# should start with just stats.

xxx

# and now I have writer/director vars.
# def use that
# who wrote/directed the 5 best?
# titles %>% arrange(-rating) %>% head(5) %>% select(title, writers) %>% unnest()

# just bind_rows a title words dataset and a "writer words" dataset 
# (e.g., last names or whatever)
titles %>% select(season, episode, word = writers) %>% unnest(word) %>% 
  mutate(word = gsub('\\s', '', word) %>% tolower(), x = 1) %>% group_by(season, episode) %>% 
  spread(key = word, value = x, fill = 0)
# why is it dropping one. w/e. ok done.



w <- titles %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>% 
  mutate(word = gsub("'s", "", word), ) %>% # replace Joey's with Joey, etc.
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

# do lm with S/E/C
# lm(rating ~ season + episode, data = w) %>% summary()
# only 25%! ok.
# season / episode / character
sec_mod <- lm(rating ~ season + episode + chandler + joey + monica + phoebe + ross + rachel, data = w)
# all relative to the pilot.
# ok, Joey sucks, Phoebe sucks, Ross is great (alarm bells should be going off bc Ross is trash)
sec_mod %>% tidy() %>% .[-1, ] %>% as_tibble() %>% filter(p.value < 0.1) %>% arrange(-abs(estimate))
# E21, E20 sucks, S4, S5, S10 great. Phoebe, Joey suck
# also only explains 25% of variation in ratings.
# `sec_mod %>% summary() %>% .$r.squared %>% `*`(100) %>% format(digits = 0) %>% paste0("%")`
# broom it to see some patterns. want significant results, 



# ok cool, cool cool. get better names for these too.
x <- sparse.model.matrix(~ ., w[,-1])[,-1]

# now what next. glmnet with penalty.

# nvars;
# penalty = 0 for the constant 
# number of seasons +
# number of different episodes + 
# 6
n_keep_vars <- max(titles$season) + max(titles$episode) + 6
penalty.factor <- c(rep(0, n_keep_vars), rep(1, ncol(x) - n_keep_vars))

# no penalty.
xxx <- cv.glmnet(x, w$rating) %>%
  coef(s = 'lambda.min') %>%
  as.matrix() %>%
  as_tibble() %>%
  mutate(var = dimnames(c)[[1]]) %>% 
  select(var, coef = `1`)
# x[, dimnames(x)[[2]] %in% (xxx[-1, ] %>% filter(!near(coef, 0)) %>% pull(var))] %>% dim()

# hmm. 90%, btr.
# write function to do post-lasso estimation.
lm(rating ~ ., data = bind_cols(w[, "rating"], x[, dimnames(x)[[2]] %in% (xxx[-1, ] %>% filter(!near(coef, 0)) %>% pull(var))] %>% as.matrix() %>% as_tibble())) %>% summary()


cvfit <- cv.glmnet(x, w$rating, penalty.factor = penalty.factor)
plot(cvfit)
c <- coef(cvfit, s = "lambda.min")



# dimnames(c)
coefs <- c %>% 
  as.matrix() %>% as_tibble() %>%
  mutate(var = dimnames(c)[[1]]) %>% select(var, coef = `1`)

# include sec_mod
# inner_join(coefs1[-1, ], clm[-1, ], by = c("var" = "term")) %>% filter(!grepl("season|episode", var)) %>%  ggplot(aes(x = coef, y = estimate)) + geom_point()
library(ggrepel)
compare_coefs <- inner_join(coefs[-1, ], sec_mod %>% tidy(), by = c("var" = "term")) %>% 
  mutate(var = gsub("season", "S", var),
         var = gsub("episode", "E", var),
         var = ifelse(var != "E11", gsub("1$", "", var), var), 
         var = stringr::str_to_title(var))
compare_coefs %>%  ggplot() + 
  geom_point(aes(x = coef, y = estimate, colour = "red")) + 
  geom_text_repel(aes(coef, estimate, label = var)) +
  # theme_classic() + 
  labs(title = "Compare coefficients from `lm` and `glmnet`",
       x = "glmnet", y = 'lm') + 
  theme(legend.position="none")
# ok cool. now...write x/y axis? and that it?

# not the same, but close.
# anyway, let's focus on the post-selection lm.
e <- x[, dimnames(x)[[2]] %in% (coefs[-1, ] %>% filter(!near(coef, 0)) %>% pull(var))] #%>% dim()
# try to get a better way to do this.
new <- bind_cols(w[, "rating"], e %>% as.matrix() %>% as_tibble())
lm(rating ~ ., data = new) %>% summary()


# note, this isn't as good as post-lasso estimation on the full set (R^2^ 94%-ish)
# but anyway, cool, look at this.

# what to say about it. look at coefficients, etc. what makes a good episode?
