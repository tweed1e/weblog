library(tidyverse)
library(tidytext)

# todo: focus on generation.
# fix Vegas problem.


titles <- werfriends::friends_episodes %>% select(-director, -writers)

# overall
title_words <- titles %>% unnest_tokens(word, title) #%>% anti_join(stop_words) # except where/with/etc. 

# also rachel, rachel's, etc.
title_words %>% mutate(word = gsub(pattern = "'s", replacement = "", x = word)) %>% count(word, sort = TRUE)
# ok, now we know what's important
# the characters, weddings, people and things dying.
titles %>% filter(grepl("dies", title, ignore.case = TRUE))
# dang, ok. old yeller and 5 old people. whoops, 4 old people, old yeller and JOEY'S CHAIR

# dplyr. makes it easy to think through a problem and get a solution right away
# but can be hard to understand afterward, maybe because you do it so quickly you
# don't bother to document it?

word_transitions <- titles %>% 
  unnest_tokens(word, title) %>% 
  group_by(season, episode) %>% 
  filter(row_number() > 1) %>% 
  mutate(nxt = lead(word), 
         nxt = ifelse(is.na(nxt), "EOL", nxt)) %>% 
  group_by(word, nxt) %>% 
  count() %>% 
  group_by(word) %>% 
  mutate(weight = n / sum(n))

generate_title <- function() {
  new_title <- c("the", "one")
  while(new_title[length(new_title)] != "EOL") {
    new_title <- c(new_title, 
                   word_transitions %>% 
                     filter(word == new_title[length(new_title)]) %>% 
                     sample_n(size = 1, weight = weight) %>% 
                     pull(nxt))
  }
  new_title[-length(new_title)] %>% paste(collapse = " ") %>% stringr::str_to_title()
}
# generate_title() %>% print() #%>% rsay::speak(voice = 'Tessa')
replicate(5, generate_title())
