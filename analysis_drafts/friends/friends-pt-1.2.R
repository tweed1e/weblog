
library(tidyverse)
library(rvest)
library(skimr)
library(ggplot2)
library(ggrepel)
library(tidytext)


get_ep_data <- function(ep_url, url) {
  # given a show url (for the session), and an episode url (relative to the show url) 
  # follow the link to the episode and download the episode data
  print(paste0("S:", stringr::str_extract(url, "(\\d*)$"), " E:", stringr::str_extract(ep_url, "(\\d*)$")))
  print(ep_url)
  
  session <- html_session(paste0(url)) %>%  jump_to(ep_url)
  page <- session %>% read_html()
  title <- page %>% html_nodes("div h1[itemprop=\"name\"]") %>% html_text() %>% trimws()
  rating <- page %>% html_nodes('span[itemprop="ratingValue"]') %>% html_text() %>% as.numeric()
  n_ratings <- page %>% html_nodes('span[itemprop="ratingCount"]') %>% html_text() %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
  
  # get writer / director info.
  cast_table <- session %>% follow_link(i = "See full cast & crew") %>%
    html_nodes(".simpleCreditsTable") %>% 
    html_table()
  
  director <- cast_table %>% .[[1]] %>% .[,1] # the director is first in the list of tables.
  
  # nope! sometimes it says "teleplay by" and "story by". use those too.
  writers <- cast_table %>% .[[2]] %>% # I think the writers are in the second table in the list of tables?
    filter(grepl("writer|written|teleplay|story", X3)) %>%  .[,1]
  
  tibble(
    season = stringr::str_extract(url, "(\\d*)$") %>% as.numeric(), #substring(url, nchar(url)), 
    episode = stringr::str_extract(ep_url, "(\\d*)$") %>% as.numeric(), #substring(ep_url, nchar(ep_url)), 
    title = title, 
    rating = rating, 
    n_ratings = n_ratings, 
    director = lst(director), 
    writers = lst(writers)
  )
}

get_season_data <- function(url) {
  # given the show url, download episode data for every episode in every season
  
  html <- read_html(url)
  
  # get links to all episodes.
  ep_list <- html %>% html_nodes(css = 'strong a[itemprop="name"]') %>% html_attr('href')
  
  # for each episode in the list, safely get the data
  safe_get_ep_data <- possibly(get_ep_data, otherwise = NULL)

  # return a dataframe of all episode data
  ep_list %>% map(safe_get_ep_data, url = url) %>% bind_rows() 
}

url <- "http://www.imdb.com/title/tt0108778/episodes?season="

urls <- map_chr(1:10, function(n) {paste0(url, n)})
titles <- urls %>% map(get_season_data) %>% bind_rows()

# saveRDS(titles, 'analysis_drafts/friends_episodes.RDS')

skim(titles)
# View(titles)
# need to unnest directors and writers separately 
# or just dorp that one director wtf is he doing there
# now get data on things. titles, ratings, n_ratings, succesful directors, writers, etc.


# titles %>% unnest(director) %>% count(director, sort = TRUE) %>% head() # ggplot or something
# titles %>% unnest(writers) %>% count(writers, sort = TRUE) %>% head() # ggplot or something

# titles %>% 
#   unnest(director) %>%
#   mutate(director = factor(director) %>% forcats::fct_reorder(rating, fun = median, .desc = FALSE)) %>% 
#   group_by(director) %>%
#   add_count() %>% 
#   ggplot() + geom_boxplot(aes(x = director, y = rating, group = director), outlier.colour = "red") + coord_flip()
  
# and this needs to be vectorized.
# get_lastname_unvec <- function(name) {
#   l <- stringr::str_split(name, " ", simplify = TRUE)
#   # if Kevin S. Bright, return Bright
#   # if Kevin S. Bright Jr., return Bright Jr.
#   # if Kevin De Bright, return De Bright. but forget about that for now.
#   l[1, ncol(l)]
# }
# get_lastname <- Vectorize(get_lastname_unvec)
# what I really want. avg rating vs. number of eps
# stats <- titles %>% 
#   unnest(director) %>%
#   group_by(director) %>%
#   summarize(n = n(), rating = mean(rating)) %>% summarize(n = median(n), rating = median(rating))

titles %>% 
  unnest(director) %>%
  group_by(director) %>%
  summarize(n = n(), rating = mean(rating)) %>% 
  ggplot() + geom_point(aes(x = n, y = rating, colour = 'red')) + 
  # geom_hline(yintercept = mean(titles$rating), linetype = 'dotted') + 
  geom_text_repel(aes(n, rating, label = director)) +
  labs(title = "Directors: Avg. rating vs. No. of episodes",
       x = "No. of eps", y = 'Avg. rating') + 
  theme(legend.position="none")

# benefits. see outliers, general pattern---no relation between episode success and no of eps.
# drawbacks: harder to investigate each director, boxplot better for that.

# titles %>% 
#   unnest(writers) %>%
#   group_by(writers) %>%
#   summarize(n = n(), rating = mean(rating)) %>% 
#   mutate(writers_last = get_lastname(writers)) %>% 
#   ggplot() + geom_point(aes(x = n, y = rating, colour = 'red')) + 
#   geom_text_repel(aes(n, rating, label = writers_last)) +
#   labs(title = "Writers: Avg. rating vs. No. of episodes",
#        x = "No. of eps", y = 'Avg. rating') + 
#   theme(legend.position="none")
# maybe put those together into facets or grid plot or w/e

# kay, what else do I want to talk about. just get the data etc.
# talk about it. then go to titles + schwimmer
# david schwimmer is a director, what did he do?
# titles %>% filter(director != "David Schwimmer") %>% pull(rating) %>% mean()

# surprising.
titles %>% 
  unnest(director) %>% 
  filter(director == "David Schwimmer") 
# not terrible, really. surprising. I assumed he'd direct all the ones about Ross.

# ok, great, now we're thinking about titles: what are the most common words?

titles %>% 
  select(-director, -writers) %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>% 
  filter(! word %in% c('1', '2')) %>% 
  mutate(word = gsub("'s", "", word)) %>% 
  group_by(word) %>% 
  summarize(n = n(), rating = mean(rating) %>% round(2)) %>%
  arrange(-n) %>% head(6)


xxx
# titles %>% 
#   select(-director, -writers) %>%
#   group_by(title) %>% 
#   unnest_tokens(word, title, drop = FALSE) %>% 
#   ungroup() %>% 
#   mutate(word = gsub("'s", "", word)) %>% 
#   inner_join(tibble(word = c("rachel", "ross", "monica", "chandler", "joey", "phoebe"))) %>%
#   mutate(word = stringr::str_to_title(word)) %>% 
#   group_by(word) %>% 
#   ggplot(aes(x = word, y = rating, group = word)) + geom_boxplot() +# geom_jitter(aes(colour = 'red')) +
#   coord_flip()

# hmm---definite ranking in number of episodes terms, but not success. hmm.

# xxx

# titles %>% 
#   unnest(writers) %>%
#   mutate(writers = factor(writers) %>% forcats::fct_reorder(rating, fun = median, .desc = TRUE)) %>% 
#   group_by(writers) %>%
#   add_count() %>% 
#   ggplot() + geom_boxplot(aes(x = writers, y = rating, group = writers), outlier.colour = "red")

# titles %>% ggplot(aes(x = rating, y = n_ratings)) + geom_point() + scale_y_log10()

# writers by season?
# titles %>% unnest(writers) %>% 
#   mutate(season = as.numeric(season)) %>% 
#   distinct(writers, season) %>% mutate(x = 1) %>% complete(writers, season) %>%
#   mutate(season = ifelse(is.na(x), NA, season)) %>% arrange(desc(writers), season) %>% 
#   ggplot(aes(x = season, y = writers, group = writers)) + geom_path() + geom_point() +
#   scale_x_continuous(breaks = 1:10) + 
#   labs(title = "Writers: Season patterns",
#        x = "Season", y = 'Writer') + 
#   theme(legend.position="none")
# 
# titles %>% unnest(writers) %>% 
#   mutate(season = as.numeric(season),
#          writers = factor(writers) %>% fct_reorder(season, fun = (function(x, na.rm) { -min(x) }), na.rm = TRUE)) %>% 
#   distinct(writers, season) %>% mutate(x = 1) %>% complete(writers, season) %>%
#   mutate(season = ifelse(is.na(x), NA, season)) %>% 
#   ggplot(aes(x = season, y = writers, group = writers)) + geom_path() + geom_point() +
#   scale_x_continuous(breaks = 1:10) + 
#   labs(title = "Writers: Season patterns",
#        x = "Season", y = 'Writer') + 
#   theme(legend.position="none")
# 
# 
# titles %>% unnest(director) %>% 
#   mutate(season = as.numeric(season),
#          director = factor(director) %>% fct_reorder(season, fun = (function(x, na.rm) { -min(x) }), na.rm = TRUE)) %>% 
#   distinct(director, season) %>% mutate(x = 1) %>% complete(director, season) %>%
#   mutate(season = ifelse(is.na(x), NA, season)) %>% 
#   ggplot(aes(x = season, y = director, group = director)) + geom_path() + geom_point() +
#   scale_x_continuous(breaks = 1:10) + 
#   labs(title = "Directors: Season patterns",
#        x = "Season", y = 'Director') + 
#   theme(legend.position="none")


