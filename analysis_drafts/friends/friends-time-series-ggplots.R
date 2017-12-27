# Friends time series things

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)



episodes <- werfriends::friends_episodes
episodes
# hi.

# episodes <- episodes %>% mutate(SE = paste0("S", season, ";E", episode))

episodes <- episodes %>% group_by(season, episode) %>% mutate(SE = paste0(ifelse(season > 9, "", " "), season, ifelse(episode > 9, "", "0"), episode, collapse = "")) %>% arrange(SE) %>% ungroup() %>% mutate(SE = factor(SE)) 
episodes %>% glimpse()

ggplot(episodes, aes(SE, rating, group = season, colour = factor(season))) + geom_smooth() + geom_line() + scale_x_discrete(breaks = NULL)
# looks cool. easy to pick out outliers.
# some seasonality
# ggrepel ep numbers and titles.

ggplot(episodes, aes(SE, rating)) + geom_point() + scale_x_discrete(breaks = NULL)
ggplot(episodes, aes(SE, rating, group = season, colour = factor(season))) + geom_point() + geom_line() + scale_x_discrete(breaks = NULL)
ggplot(episodes, aes(episode, rating, group = season)) + geom_line() + geom_point() + 
  facet_grid(season ~ ., scales = "fixed") #+ scale_x_discrete(breaks = NULL)

# colours <- c(rep("grey", 4), hue_pal()(1), rep("grey", 5))
colours <- c(rep("grey", 9), hue_pal()(1))
e$season <- e$season[, drop = TRUE]

# factor(e$season)
# levels(e$season) <- levels(e$season)[c(1:4, 6:10, 5)]
# reorder(e$season, levels(e$season)[c(1:4, 6:10, 5)])
# levels(e$season)
# e
# factor(e$season, levels = levels(e$season)[c(1:4, 6:10, 5)])
# factor(e$season, levels = levels(e$season)[c(1:7, 9:10, 8)])
# e %>% tail()

e %>% 
  mutate(season = factor(season, levels = levels(season)[c(2:8, 9:10, 1)])) %>%
  # mutate(rating_5 = ifelse(season == 5, rating, as.numeric(NA)),
  #        rating = ifelse(season == 5, as.numeric(NA), rating)) %>% 
  ggplot() + 
    geom_line(aes(episode, rating, group = season, colour = season, size = 0.5)) + 
    # geom_line(aes(episode, rating_5, group = season, colour = season)) + 
    scale_colour_manual(values = colours) + # c(rep(hue_pal()(1), 10))) + 
    # scale_x_continuous(breaks = c(1:25)) +
  guides(colour = FALSE, size = FALSE) + 
      labs(x="Episode",
           y="Rating",
           title = "Friends episode ratings by season",
           subtitle = "Season 1 highlighted") + theme_minimal()


# pretty ugly. can see some pattern, but tough, especially to compare across seasons that
# aren't next to each other.
eps <- episodes %>% 
  mutate(season = factor(str_pad(season, width = 2), 
                         levels = c(str_pad(c(1:10), width = 2), "smooth")))
# e <- eps %>% 
#   group_by(season) %>% 
#   mutate(rating_out = ifelse(rating == min(rating) | rating == max(rating), rating, NA)) %>% 
#   group_by(season, episode) %>% 
#   mutate(label = paste0("S", trimws(season), "E", episode, collapse = "")) %>% 
#   ungroup()

e <- eps %>% 
  group_by(season) %>%
  mutate(rating_out = ifelse(rating == min(rating) | rating == max(rating), rating, NA)) %>%
  group_by(season, episode) %>% 
  mutate(label = paste0("S", trimws(season), "E", episode, collapse = ""),
         label = ifelse(label %in% c("S4E21", "S6E20", "S7E21", "S8E19", "S9E10", "S10E10"), label, "")) %>% ungroup()

e

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


# mtcars %>%
#   group_by(cyl) %>%
#   mutate(outlier = ifelse(is_outlier(drat), drat, as.numeric(NA))) %>%
#   ggplot(., aes(x = factor(cyl), y = drat)) +
#   geom_boxplot() +
#   geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

# cool, this one the best for comparing stats across seasons.
e %>% 
  group_by(season) %>% 
  # mutate(outlier = ifelse(is_outlier(rating), rating, as.numeric(NA))) %>% 
  ggplot(aes(season, rating, group = factor(season))) +
  geom_boxplot() + 
  geom_text_repel(aes(label = ifelse(is_outlier(rating), title, "")))
# +
  # scale_x_continuous(breaks = c(1:10))
# season 5 da best, medians go up from 1-5, then down, then back up at last season.
# easy to pick out outliers. Really picks out important bits---answer Q, but also easy 
# to ask questions about outliers. no seasonality tho.


# kinda cool. if can pick out seasonal patterns, but need to worry about
# colour scale spectrum problems
ggplot(episodes, aes(season, episode, fill = rating)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="grey", high="red") +
  scale_x_continuous(breaks = c(1:10)) + 
  labs(x="Season",
       y="Episode",
       title = "Friends episode rating calendar heatmap", 
       fill="Rating")

# library(RColorBrewer)
# aes(colour = factor(season))


p <- ggplot(e, aes(SE, rating, group = season, colour = season)) + 
  geom_smooth(aes(colour = "smooth")) + geom_line() + geom_point(aes(SE, rating_out)) +
  # scale_colour_manual(values = c(brewer_pal("qual")(9), brewer_pal("qual")(1), "#aaaaaa")) +
  scale_colour_manual(values = c(hue_pal()(10), "#aaaaaa")) + 
  guides(colour = FALSE) +
  theme_minimal()
  

paste0("S:", 1:10)
#breaks = c(paste0(str_pad(c(1:10), width = 2), "01", collapse = ""), labels = c(paste0(str_pad(c(1:10), width = 2), "01", collapse = ""))) +
p + scale_x_discrete(breaks = paste(str_pad(c(1:10), width = 2), "10", sep = ""), labels = paste0("S:", 1:10)) 


p + geom_text_repel(aes(label = e$label)) + 
  labs(title = "Friends IMDB ratings over time", subtitle = "Clip shows labelled. lmao", x = "Season, episode", y = "Rating")
# good, or make sure max only returns one, 
# or list the titles, or list only the clip shows

e %>% arrange(rating) %>% head(5) # i htink.

xxx

# SE = paste0("S", season, ";E", episode)

# ?ggrepel::geom_text_repel
# eps <- episodes %>% 
#   mutate(season = factor(str_pad(season, width = 2), 
#                          levels = c(str_pad(c(1:10), width = 2), "smooth")))
# eps
# ggplot(eps, aes(SE, rating, group = season, colour = season)) + 
#   geom_smooth(aes(colour = "smooth")) + geom_line() + #geom_point() +
#   scale_colour_manual(values = c(rep(viridis_pal(option = "D")(6)[-6], 2), "#aaaaaa")) + # for 
#   guides(colour = FALSE) + 
#   scale_x_discrete(breaks = NULL) 


# ?RColorBrewer::brewer.pal
# show_col(brewer_pal()(9))

# ggplot(episodes %>% mutate(colour = factor(season %% 2)), aes(SE, rating, group = season)) + 
#   geom_smooth(aes(colour = 'other')) + geom_line(aes(colour = colour)) + 
#   scale_colour_manual(values = c('red', 'blue', 'grey')) + 
#   scale_x_discrete(breaks = NULL) # do these better.

# ?scale_colour_brewer
# scale_colour_brewer()

# library(scales)
# colour_ramp()
# show_col(viridis_pal()(10))
# viridis_pal()(10) # ok cool. but the regular one.
# scales::area_pal()(2)
# cool. this took a long time. hue_pal()(4)

