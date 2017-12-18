library(babynames)
library(tidyverse)

drop_vowels <- function(text) {
  gsub("[aeiou]", "", text, ignore.case = TRUE) # apply some function to the text; gsub or the like
}
add_bang <- function(text) {
  paste0(text, "!")
}
# mutate_names <- function(tbl, var = "name") {
#   # names are cooler with no vowels and no capital letters.
#   dplyr::mutate(tbl, !!rlang::sym(var) := tolower(add_bang(drop_vowels(!!rlang::sym(var)))))
# }

mutate_names_ <- function() { 
  add_bang <- add_bang
  drop_vowels <- drop_vowels
  function(tbl) {
    # names are cooler with no vowels and no capital letters.
    dplyr::mutate(tbl, name := tolower(add_bang(drop_vowels(name))))
  }
}

cl <- parallel::makeCluster(2)
# parallel::clusterExport(cl, c("add_bang", "drop_vowels"))
bind_rows(parallel::parLapply(cl, split(babynames, babynames$year), mutate_names_()))
parallel::stopCluster(cl)
