
# LIBRARIES & IMPORT DATA -------------------------------------------------

library(dplyr)
setwd("~/Documents/data-science-roles")

file_path <- 'data/indeed_job_dataset.csv'
df <- read.csv(file = file_path, stringsAsFactors = F) %>% 
  dplyr::rename(id="X")

# Description Exploration -------------------------------------------------
i <- 100
df$Job_Type[i]
df$Job_Title[i]
df$Description[i]
df$Skill[i]

cleanHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

mystopwords <- tibble(word = c("\n"))

df_words <- df %>% 
  dplyr::select(id, Job_Title, Job_Type, Description) %>% 
  # dplyr::slice(1:20) %>%
  dplyr::mutate(Description = cleanHTML(Description)) %>% 
  tidytext::unnest_tokens(word, Description) %>% # tokenize
  dplyr::anti_join(tidytext::stop_words, mystopwords, by="word") %>% # remove stopwords 
  dplyr::mutate(word = stringr::str_extract(word, "[a-z']+")) %>% # get only words
  dplyr::filter(!is.na(word)) # remove NA

df_words %>%
  dplyr::count(word, sort = TRUE) %>%
  with(wordcloud::wordcloud(word, n, max.words = 100, rot.per = 0, scale = c(5,1), random.order = F))

# Word Count --------------------------------------------------------------

df_words %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 15) %>%
  dplyr::top_n(50, n) %>% 
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(x=word, y=n), na.rm=T) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip()

df_words %>%
  dplyr::group_by(Job_Type) %>% 
  dplyr::count(word, sort = TRUE) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(n > 15) %>%
  dplyr::top_n(50, n) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(x=word, y=n, fill=Job_Type), na.rm=T) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(cols=vars(Job_Type))+
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip()


# Sentiment Analysis ------------------------------------------------------

# sentiment_data <- dplyr::bind_rows(
#   tidytext::get_sentiments("afinn"),
#   tidytext::get_sentiments("bing"),
#   tidytext::get_sentiments("loughran"),
#   tidytext::get_sentiments("nrc")
# )
df_words %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by="word") %>% 
  dplyr::count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("red4", "seagreen"), 
                              rot.per = 0, scale = c(5,1), max.words = 100)


# tf-idf ------------------------------------------------------------------

# term frequency (tf), how frequently a word occurs in a document
# inverse document frequency (idf), decreases the weight for commonly used words 
freq_words <- df_words %>%
  dplyr::count(Job_Type, word, sort = TRUE)

total_words <- freq_words %>%
  dplyr::group_by(Job_Type) %>% 
  dplyr::summarise(total = sum(n))

freq_words <- dplyr::left_join(freq_words, total_words, by="Job_Type")
freq_words %>% 
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(n/total, fill = Job_Type),
                          na.rm=T, show.legend = FALSE) +
  ggplot2::xlim(NA, 0.0009) +
  ggplot2::facet_grid(cols=vars(Job_Type), scales = "free_y")

# Zipf's Law: the frequency that a word appears is inversely proportional to its rank
freq_words %>% 
  dplyr::group_by(Job_Type) %>% 
  dplyr::mutate(rank = dplyr::row_number(), 
                `term frequency` = n/total) %>% 
  ggplot2::ggplot(ggplot2::aes(rank, `term frequency`, color = Job_Type)) + 
  ggplot2::geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10()

df_words %>%
  dplyr::count(Job_Type, word, sort = TRUE) %>% 
  tidytext::bind_tf_idf(word, Job_Type, n) %>%
  dplyr::arrange(desc(tf_idf)) %>%
  dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  dplyr::group_by(Job_Type) %>% 
  dplyr::top_n(15, tf_idf) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = Job_Type)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::labs(x = NULL, y = "tf-idf") +
  ggplot2::facet_wrap(~Job_Type, nrow = 1, scales = "free") +
  ggplot2::coord_flip()


# N-grams and correlations ------------------------------------------------

mystopwords <- tibble(word = c("\n"))
stop_words <- dplyr::bind_rows(tidytext::stop_words, mystopwords)

df_bigrams <- df %>% 
  dplyr::select(id, Job_Title, Job_Type, Description) %>% 
  # dplyr::slice(1:20) %>%
  dplyr::mutate(Description = cleanHTML(Description)) %>% 
  tidytext::unnest_tokens(bigram, Description, token="ngrams", n=2)

df_bigrams %>% 
  dplyr::count(bigram, sort = TRUE)

df_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word) %>% 
  dplyr::count(word1, word2, sort = TRUE) %>%
  tidyr::unite(bigram, word1, word2, sep = " ")

df_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word) %>% 
  dplyr::count(Job_Type, word1, word2, sort = TRUE) %>%
  tidyr::unite(bigram, word1, word2, sep = " ") %>%
  # tidytext::bind_tf_idf(bigram, Job_Type, n) %>%
  dplyr::group_by(Job_Type) %>% 
  dplyr::top_n(10) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Job_Type, n) %>%
  dplyr::mutate(bigram = reorder(bigram, n)) %>%
  ggplot2::ggplot(ggplot2::aes(bigram, n, order=n, fill = Job_Type)) +
  ggplot2::geom_col(show.legend = FALSE) +
  # ggplot2::labs(x = NULL, y = "tf-idf") +
  ggplot2::facet_wrap(~Job_Type, nrow = 1, scales = "free") +
  ggplot2::coord_flip()

