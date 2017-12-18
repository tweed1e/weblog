# To start: train something to generate friends episode names
# script to get the names, first.
# via imdb, probably.

# Motivation for these posts
# Most NLP or text analysis blogs are two types: 
# super simple (e.g., most common words, analyze sentiments)
# or super complex (LSTM model for document generation or something)

# I want to do one in between
# (a) readily available data, accessible topic (e.g., tv)
# (b) more complex than "what are the words?"
# (c) but you don't have to write an NLP thesis to understand

# answer: friends scripts. 
# first: titles---how to get them, analyze them, 
# and can I generate a friends episode title without 
# knowing markov models or LSTM or whatever
# and I want to show mistakes / data cleaning issues as they come up.

library(rvest)
library(XML) # why?
library(tidyverse)
library(tidytext)

url <- "http://www.imdb.com/title/tt0108778/episodes?season="

urls <- map_chr(1:10, function(n) {paste0(url, n)})

pages <- map(urls, read_html)

# page <- read_html(url)
# page

# nodes <- 

# pages %>% 
#   map(html_nodes, css = '.ipl-rating-star__rating') %>% .[[1]] %>% 
#   html_text() %>% 
#   enframe() 
# 
# 
# pages %>% 
#   map(html_nodes, css = '.ipl-rating-star__rating') %>% .[[1]] %>% 
#   html_text() %>% 
#   enframe() %>% filter(value != "Rate") %>% mutate(value = as.numeric(value)) %>% filter(value - 1 != lag(value) ) %>% filter(value>0)
# ugh, 23.

# stars <- 
  pages %>% 
  map(html_nodes, css = '.ipl-rating-star__rating')%>% 
  map(html_text) %>% 
  enframe() %>%
  unnest() %>% 
  group_by()
    
      mutate(value = map(value, value != "Rate"))

  # function that returns...ugh. or group_by
  
# ugh, finally. now do this to every one.
stars %>% filter(name %in% seq.int(1, max(name), by = 23))


  
pages %>% 
  map(html_nodes, css = '.ipl-rating-star__rating') %>% .[[1]] %>% 
  html_text() %>% 
  enframe() %>% View()

# %>% 
#   filter(grepl("[0-9]\\.[0-9]", value))

# some are only one number. ugh.  

pages %>% 
  map(html_nodes, css = '.ipl-rating-widget') %>% 
  .[[1]]  %>% html_children() %>% `[[`(1) 
# ok, get .ipl-rating-star__rating  and
# .ipl-rating-star__total-votes
# and 

pages %>% 
  map(html_nodes, css = 'div div') %>% 
  .[[1]]  %>% `[[`(4)
  # html_nodes(css = 'div span')

# %>% 
#   map(html_text) %>% 
#   enframe() %>% 
#   unnest() %>% 
#   filter(grepl("[0-9]\\.[0-9]", x = value))
  
titles <- pages %>% 
  map(html_nodes, css = 'strong a') %>% 
  map(html_text) %>% 
  enframe() %>% 
  unnest() %>%
  rename(season = name, title = value) %>% 
  filter(grepl("^The One", title)) %>% 
  group_by(season) %>% 
  mutate(episode = row_number())

titles <- titles %>% select(season, episode, title)
titles

# overall
title_words <- titles %>% unnest_tokens(word, title) %>% anti_join(stop_words) 

title_words %>% ungroup() %>% count(word, sort = TRUE)

# part 1, 2?
# figure that out.
titles %>% filter(grepl("[0-9]", title))
# ok. hmm. decision: leave it for now. but note: lots of season finales

# seasons that end with two-parters
titles %>% 
  filter(episode == max(episode)) %>%
  filter(grepl("Part 2", title)) %>% 
  pull(season)

# note: switching the two filters gives the wrong answer,
# bc filtering for 'Part 2' first changes the logic for `max()`
titles %>% 
  filter(grepl("Part 2", title)) %>% 
  filter(episode == max(episode)) %>%
  pull(season)

# but you can combine them with `&`
titles %>% 
  filter(grepl("Part 2", title) & episode == max(episode)) %>%
  pull(season)


# also rachel, rachel's, etc.
title_words %>% mutate(word = gsub(pattern = "'s", replacement = "", x = word)) %>% ungroup() %>% count(word, sort = TRUE)
# ok, now we know what's important
# the characters, weddings, people and things dying.
titles %>% filter(grepl("dies", title, ignore.case = TRUE))

# dang, ok. old yeller and 5 old people. whoops, 4 old people, old yeller and JOEY'S CHAIR

# well, they all start with "The One"
# but the third word changes.
titles %>% mutate(third = strsplit(title, " ")[[1]][[3]]) 

# that didn't work. why? ugh. maybe the groups give it trouble?
titles %>% group_by(season, episode) %>% mutate(third = strsplit(title, " ")[[1]][[3]]) 
# yeah, I guess group_by can fix that. can get the same thing by grouping by title.

titles %>% 
  group_by(season, episode) %>% 
  mutate(third = strsplit(title, " ")[[1]][[3]] %>% tolower()) %>% 
  ungroup() %>% 
  count(third, sort = TRUE)

# ok, mainly with. 
# notice "hundredth"

# is there a season component?
titles %>% 
  group_by(season, episode) %>% 
  mutate(third = strsplit(title, " ")[[1]][[3]] %>% tolower()) %>% 
  group_by(season) %>% 
  count(third, sort = TRUE)

# verbs, nouns, etc?

# medium term thing: also get ratings, try to predict ratings given titles?
# that's some NLP stuff. features are important.
# characters, obviously
# writers maybe more important but whatever.

# so how to generate a sentence?
# markov model.
# but to start, just think
# The One {with, where, after} {noun} {if possesive noun, another (adjective noun), otherwise {verb} then (adjective noun)}

pos <- title_words %>% mutate(word = gsub(pattern = "'s", replacement = "", x = word)) %>% left_join(parts_of_speech)
# ok, but multiples; pick most obvious? hmm. usually just nouns. NA are usually nouns.

# but anyway, get a set of nouns
# lots.
pos %>% filter(is.na(pos) & !grepl("[0-9]", word)) %>% ungroup() %>% count(word, sort = TRUE)
# hmm, some reasonable things in here. hard to tell the difference between verbs "hates" and noun "steaks"
# unless we strip plural, and try again?
# forget it, for now.

pos <- pos %>% filter(!is.na(pos)) %>% group_by(pos, word) %>% count()

# ok cool. but wtf are the parts of speech?
pos %>% ungroup() %>% count(pos)

# lots of nouns, then verbs, then adjectives
# transitive vs intransitive. ugh.

# can use this to analyze parts of speech for titles though. e.g., "The One {with, after, etc} {noun} {verb} etc.
# how to analyze that and visualize it?
# sankey type thing. probably tree data, right? get links like 
# Noun (edge weight = count)-> Adverb

# so, how to get "edges"? or translate things already
# go back to original, put stop words back in
titles %>% 
  unnest_tokens(word, title) %>% 
  group_by(season, episode) %>% 
  mutate(lineno = row_number()) %>% 
  mutate(word_ = gsub(pattern = "'s", replacement = "", x = word)) %>% 
  left_join(parts_of_speech, by = c("word_" = "word"))
# hmm. that's tough. now I need to pick.

# what's bad?
titles %>% 
  unnest_tokens(word, title) %>% 
  group_by(season, episode) %>% 
  mutate(lineno = row_number()) %>% 
  mutate(word_ = gsub(pattern = "'s", replacement = "", x = word)) %>% 
  left_join(parts_of_speech, by = c("word_" = "word")) %>% group_by(word_, pos) %>% 
  slice(1) %>% ungroup() %>% count(word_, sort = TRUE) %>% filter(n>1) %>% nrow()
# ugh. 158 choices to make. 
# don't know what to do.
# ok. strategy? use tf-idf to pick what's the best one to use.

# which means. for each thing.
parts_of_speech %>% filter(word == "the") # two types
parts_of_speech %>% filter(pos == "Adverb") # ~13k adverbs
parts_of_speech %>% filter(pos == "Definite Article") # but like ~100 definite articles

# ok, try bind_tf_idf?
pos_ <- parts_of_speech %>% mutate(n = 1) %>% bind_tf_idf(word, pos, n)
pos_ %>% filter(word == "the") %>% group_by(word) %>% arrange(-tf_idf) # ok, good
pos_ %>% filter(word == "like") %>% group_by(word) %>% arrange(-tf_idf)

# pos_ %>% group_by(word) %>% arrange(-tf_idf) %>% slice(1) # or:
pos_ <- pos_ %>% group_by(word) %>% filter(tf_idf == max(tf_idf)) %>% select(-(n:tf_idf))

# pos_ %>% group_by(word) %>% count(sort = TRUE)
# xxx
# so it's probably a definite article. use that rule, change in some cases. e.g.:
titles %>% filter(grepl("like", title, ignore.case = TRUE)) # dang, it's really a verb here.

# ok, try to merge parts of speech again.
titles_ <- titles %>% 
  unnest_tokens(word, title) %>% ungroup() %>% 
  filter(!(word %in% c("part", "1", "2"))) %>%
  group_by(season, episode) %>% 
  mutate(lineno = row_number()) %>% 
  mutate(word_ = gsub(pattern = "'s", replacement = "", x = word)) 

titles_pos <- bind_rows(
  titles_ %>% inner_join(pos_, by = c("word_" = "word")),
  # if something ends in 's' and isn't joined, drop the 's' and try again?
  # then make it up.
  titles_ %>%
    anti_join(pos_, by = c("word_" = "word")) %>% 
    mutate(word_ = gsub(pattern = "s$", replacement = "", x = word)) %>% 
    left_join(pos_, by = c("word_" = "word")) %>% 
    mutate(pos = case_when(is.na(pos) & grepl("ing$", word) ~ "Verb (transitive)",
                           is.na(pos) & !grepl("ing$", word) ~ "Noun",
                           TRUE ~ pos))
) %>% arrange(season, episode, lineno)

# now what.
# 1. take out spaces of pos, then paste it back together

titles_pos_fin <- titles_pos %>% 
  mutate(pos = gsub("[[:space:]]", "", tolower(pos)),
         pos = ifelse(lineno <= 3, word_, pos)) %>% 
  summarise(title = paste(word_, collapse = " "),
            pos = paste(pos, collapse = " "))

# hahaha. alright cool.
titles_pos_fin %>% group_by(pos) %>% count(sort = TRUE)

# ok, now we're getting somewhere.

# now go to edges.
# use lag?
library(igraph)
graph <- titles_pos %>%
  select(-word) %>% 
  mutate(pos = ifelse(lineno <= 3, word_, pos), 
         x = lead(pos),
         x = ifelse(is.na(x), "EOL", x)) %>% 
  ungroup() %>%
  count(pos, x, sort = TRUE) %>%
  rename(word = pos, nxt = x) #%>% 
  # igraph::graph_from_data_frame() 

graph
# this is basically a markov thing.
# group by word, normalize,
g <- graph %>% filter() %>% group_by(word) %>% mutate(weight = n / sum(n))

# then generate sentence like that, include end? like Noun -> .
# markov thing.

# basically start with "the one", draw from the set according to
# sample ... probs = c(vector defined by weight)
# just convert this to named matrix?

new_title <- c("the")
while( new_title[length(new_title)] != "EOL") {
  new_title <- c(new_title, 
                 g %>% 
                   filter(word == new_title[length(new_title)]) %>% 
                   sample_n(size = 1, weight = weight) %>% 
                   pull(nxt))
}
new_title
# then, use pos data to draw those at random?

q <- new_title %>% enframe()
q %>% 
  left_join(titles_pos %>% ungroup() %>% count(word_, pos) %>% mutate(weight = n / sum(n)), by = c("value" = "pos")) %>% 
  mutate(weight = ifelse(is.na(weight), 1, weight)) %>% 
  group_by(name) %>% sample_n(size = 1, weight = weight) %>% 
  mutate(word = ifelse(is.na(word_), value, word_)) %>% 
  filter(word != "EOL") %>% ungroup() %>% 
  summarize(title = paste(word, collapse = " ") %>% stringr::str_to_title()) %>% 
  pull(title)

# "The One With The Joey Rachel Nap"
# "The One With The Wedding Doesn't Dies" # e.g., here, it thinks 'doesn't' is a noun, bc I guessed at most of the NA ones.
# "The One With The Male Rachel"

# most common title: "The One With The [Character] [Character] Dies"
# yes! success!
# what we learned. rarely gets grammar details correct. 
# maybe better "markov model thing" (if that's what I did) works? 
# and might be easier, since we don't have to classify things?
# More discussion on that: problem: not enough variation, e.g., once it hits
# "After", it'll always say "The Superbowl" (but find better example)
# So collapsing a step in between to say "adverb" "noun" can give you more options
# Other way would be ok if we were ok reproducing original titles or we had enough
# training examples to have more variation. 
# so extensions: eliminate classification step, train on full scripts-ish?

# Fix thing where the dropped 's' don't come back; e.g., "vega" instead of "vegas"


# now split this up into manageable chunks. 
# all sitcoms recycle their ideas.
# so just recycle it to get more episodes.
# need to 
