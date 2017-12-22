
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

skim(titles %>% select_if(is.numeric))
# View(titles)
# need to unnest directors and writers separately 
# or just dorp that one director wtf is he doing there
# now get data on things. titles, ratings, n_ratings, succesful directors, writers, etc.

# titles %>%
#   unnest(director) %>%
#   mutate(director = factor(director) %>% forcats::fct_reorder(rating, fun = median, .desc = FALSE)) %>%
#   group_by(director) %>%
#   add_count() %>%
#   ggplot() + geom_boxplot(aes(x = director, y = rating, group = director), outlier.colour = "red") + coord_flip()

# fontface = 'bold', color = 'white',
# box.padding = 0.35, point.padding = 0.5,
# segment.color = 'grey50'

dirs <- titles %>% 
  unnest(director) %>%
  group_by(director) %>%
  summarize(n = n(), rating = mean(rating))

notable_dirs <-c("Joe Regalbuto", 
                 "Peter Bonerz",
                 "David Schwimmer",
                 "Kevin Bright", 
                 "Gary Halvorson")

ggplot(dirs, aes(x = n, y = rating, color = director %in% notable_dirs)) + 
  geom_point() + 
  geom_text_repel(aes(label = director)) +
  labs(title = "Directors: Avg. rating vs. No. of episodes",
       x = "No. of eps", y = 'Avg. rating') + 
  theme(legend.position = "none") + 
  scale_colour_manual(values=c("black", "#ff1ac6"))

# benefits. see outliers, general pattern---no relation between episode success and no of eps.
# drawbacks: harder to investigate each director, boxplot better for that.

# surprising.
titles %>% 
  unnest(director) %>% 
  filter(director == "David Schwimmer") #%>% select(-director) %>% knitr::kable()
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

