
library(tidyverse)
library(tidytext)

titles <- werfriends::friends_episodes %>% select(-director, -writers)
# titles %>% unnest_tokens(word, title) %>% filter(grepl("'s", word)) %>% count(word)
title_words <- titles %>% 
  unnest_tokens(word, title) %>% 
  group_by(season, episode) %>% 
  mutate(lineno = row_number()) %>% 
  group_by(season, episode, lineno) %>% 
  mutate(x = ifelse(grepl("'", word), 
                    str_match(word, "([a-z]*)('s)") %>% .[,-1] %>% lst(),
                    word %>% lst())) %>% unnest() %>% 
  ungroup() %>% mutate(word = coalesce(x, word)) %>% select(-x)

# also move this dataset.
# readr unzips stuff! ty tidytext documentation.
parts_of_speech <- readr::read_tsv("~/Downloads/SUBTLEX-US_frequency_list_with_PoS_information_final_text_version.zip")
titles_pos <- title_words %>% 
  left_join(parts_of_speech %>% select(word = Word, pos = Dom_PoS_SUBTLEX)) %>% 
  mutate(pos = case_when(
    grepl("'", word) ~ word,
    is.na(pos) ~ "Noun",
    TRUE ~ pos
  )) 

# # what's the most frequent title structure?
# titles_pos %>% 
#   group_by(season, episode) %>%
#   mutate(pos = ifelse(lineno<=3, word, pos)) %>% 
#   summarize(title = paste0(word, collapse = " "),
#             pos = paste0(pos, collapse = " ")) %>% 
#   group_by(pos) %>% count(sort = TRUE)



pos_transitions <- titles_pos %>% 
  group_by(season, episode) %>%
  filter(lineno >= 2) %>% 
  mutate(pos = ifelse(lineno<=3, word, pos),
         nxt = lead(pos), 
         nxt = ifelse(is.na(nxt), "EOL", nxt)) %>% 
  group_by(pos, nxt) %>% 
  count() %>% 
  group_by(pos) %>% 
  mutate(weight = n / sum(n))

pos_transitions



word_pos_freq <- titles_pos %>% ungroup() %>% count(word, pos) %>% mutate(weight = n / sum(n))

generate_structure <- function() {
  new_title <- c("the", "one")
  while(new_title[length(new_title)] != "EOL") {
    # print(new_title[length(new_title)])
    new_title <- c(new_title, 
                   pos_transitions %>% 
                     filter(pos == new_title[length(new_title)]) %>% 
                     sample_n(size = 1, weight = weight) %>% 
                     pull(nxt))
  }
  new_title
}
# generate_structure()
# xxx
generate_title <- function() {

  new_title <- generate_structure()

  new_title %>% enframe() %>% 
    left_join(word_pos_freq, by = c("value" = "pos")) %>% 
    mutate(weight = ifelse(is.na(weight), 1, weight)) %>% 
    group_by(name) %>% sample_n(size = 1, weight = weight) %>% 
    mutate(word = ifelse(is.na(word), value, word)) %>% 
    filter(word != "EOL") %>% ungroup() %>% 
    summarize(title = paste(word, collapse = " ")) %>% 
    pull(title) %>% gsub(" 's ", "'s ", .) %>% 
    str_to_title() %>% gsub("Pbs", "PBS", .)
}
generate_title()

replicate(5, generate_title())




######## Do tidytext pos

parts_of_speech <- readr::read_tsv("~/Downloads/SUBTLEX-US_frequency_list_with_PoS_information_final_text_version.zip")
pos <- parts_of_speech
# pos <- pos %>% select(word = Word,
#                       all_pos = All_PoS_SUBTLEX, 
#                       all_freq = All_freqs_SUBTLEX) 
# or base: strsplit(), but then need group_by
# str_split better because vectorized.
# p <- 
# pos %>% 
parts_of_speech <- readr::read_tsv("data-raw/SUBTLEX-US_frequency_list_with_PoS_information_final_text_version.zip") %>% 
  mutate(code = All_PoS_SUBTLEX %>% stringr::str_split(pattern = "\\."),
         freq = All_freqs_SUBTLEX %>% stringr::str_split(pattern = "\\.")) %>% 
  unnest() %>%
  select(word = Word, code, freq) %>% 
  filter(!is.na(as.numeric(freq)))
parts_of_speech
# parts_of_speech <- p %>% select(word, code, freq) #%>% mutate(freq = as.numeric(freq))

# p %>% filter(is.na(as.numeric(freq))) %>% View()
# parts_of_speech %>% filter(Word == "invalidates") # doesn't have a thing.
# invalidates
