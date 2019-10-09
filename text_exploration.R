
# LIBRARIES & IMPORT DATA -------------------------------------------------

library(dplyr)
setwd("~/Documents/data-science-roles")

file_path <- 'data/indeed_job_dataset.csv'
df <- read.csv(file = file_path, stringsAsFactors = F)

# Description Exploration -------------------------------------------------
i <- 100
df$Job_Type[i]
df$Job_Title[i]
df$Description[i]
df$Skill[i]

cleanHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

description_text <- df %>% 
  dplyr::select(X, Job_Title, Job_Type, Description) %>% 
  # dplyr::slice(1:20) %>%
  dplyr::mutate(Description = cleanHTML(Description)) %>% 
  tidytext::unnest_tokens(word, Description) %>% 
  dplyr::anti_join(tidytext::stop_words, by="word")


# Word Count --------------------------------------------------------------

description_text %>%
  dplyr::mutate(word = stringr::str_extract(word, "[a-z']+")) %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 15, !is.na(word)) %>%
  dplyr::top_n(50, n) %>% 
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(x=word, y=n), na.rm=T) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip()

description_text %>%
  dplyr::mutate(word = stringr::str_extract(word, "[a-z']+")) %>%
  dplyr::group_by(Job_Type) %>% 
  dplyr::count(word, sort = TRUE) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(n > 15, !is.na(word)) %>%
  dplyr::top_n(50, n) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(x=word, y=n, fill=Job_Type), na.rm=T) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(cols=vars(Job_Type))+
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip()


# Sentiment Analysis ------------------------------------------------------


