
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
  dplyr::select(id, Job_Type, Description) %>% 
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
  dplyr::count(word, Job_Type, sort = TRUE) %>%
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

mystopwords <- tibble(word = c("\n", "xi"))
stop_words <- dplyr::bind_rows(tidytext::stop_words, mystopwords)

ng <- 2
df_ngrams <- df %>% 
  dplyr::select(id, Job_Title, Job_Type, Description) %>% 
  # dplyr::slice(1:20) %>%
  dplyr::mutate(Description = cleanHTML(Description)) %>% 
  tidytext::unnest_tokens(ngram, Description, token="ngrams", n=ng) %>% 
  dplyr::mutate(ngram = stringr::str_extract(ngram, paste(rep("[a-z']+", ng), collapse = " "))) %>% # get only words
  dplyr::filter(!is.na(ngram))

df_ngrams %>% 
  dplyr::count(ngram, sort = TRUE)

df_ngrams %>%
  tidyr::separate(ngram, paste0("word", 1:ng), sep = " ") %>% 
  dplyr::filter_at(vars(starts_with("word")), all_vars(!(. %in% stop_words$word))) %>% 
  dplyr::group_by_at(vars(starts_with("word"))) %>% 
  dplyr::summarise(n=n()) %>% 
  tidyr::unite(ngram, paste0("word", 1:ng), sep = " ")

df_ngrams %>%
  tidyr::separate(ngram, paste0("word", 1:ng), sep = " ") %>% 
  dplyr::filter_at(vars(starts_with("word")), all_vars(!(. %in% stop_words$word))) %>% 
  dplyr::group_by_at(vars(Job_Type, starts_with("word"))) %>% 
  dplyr::summarise(n=n()) %>% 
  tidyr::unite(ngram, paste0("word", 1:ng), sep = " ") %>%
  tidytext::bind_tf_idf(ngram, Job_Type, n) %>%
  dplyr::group_by(Job_Type) %>% 
  dplyr::top_n(15, n) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(ngram, n, fill = Job_Type), show.legend = FALSE) +
  ggplot2::facet_wrap(~Job_Type, nrow = 1, scales = "free") +
  ggplot2::coord_flip()



# Network Representation --------------------------------------------------

set.seed(100)
ar <- grid::arrow(type = "closed", length = grid::unit(.15, "inches"))
df_ngrams %>%
  tidyr::separate(ngram, paste0("word", 1:ng), sep = " ") %>% 
  dplyr::filter_at(vars(starts_with("word")), all_vars(!(. %in% stop_words$word))) %>% 
  dplyr::group_by_at(vars(starts_with("word"))) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::filter(n > 400) %>%
  # dplyr::top_n(10, -n) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link(ggplot2::aes(edge_alpha = scales::rescale(n, c(0.15, 1)),
                                      edge_width = scales::rescale(n, c(1, 5))), 
                         show.legend = FALSE, edge_colour = "cyan4",
                         arrow = ar, end_cap = ggraph::circle(.07, 'inches')) +
  ggraph::geom_node_point(color = "black", size = 3) +
  ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1)+
  ggplot2::theme_void()


# Pairs of words with the widyr package -----------------------------------

df_words %>% 
  widyr::pairwise_count(word, id, sort = TRUE) %>% head
# Pairwise correlation: how often they appear together relative to how often they appear separately
#  phi coefficient: how much more likely it is that either both word X and Y appear, or neither do, than that one appears without the other.

word_cors <- df_words %>%
  dplyr::group_by(word) %>%
  dplyr::filter(dplyr::n() >= 1000) %>%
  widyr::pairwise_cor(word, id, sort = TRUE)

word_cors %>%
  dplyr::filter(item1 %in% c("data", "machine", "business","engineer","analyst","scientist")) %>%
  dplyr::group_by(item1) %>%
  dplyr::top_n(10) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(item2 = reorder(item2, correlation)) %>%
  ggplot2::ggplot(ggplot2::aes(item2, correlation)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(~ item1, scales = "free_y") +
  ggplot2::coord_flip()

set.seed(2016)
ar <- grid::arrow(type = "closed", length = grid::unit(.15, "inches"))
word_cors %>%
  dplyr::filter(item1 %in% c("data", "machine", "business","engineer","analyst","scientist")) %>% 
  dplyr::filter(abs(correlation) > 0.15) %>%
  igraph::graph_from_data_frame() %>%
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link(ggplot2::aes(edge_alpha = scales::rescale(correlation, c(0.15, 1))), 
                         show.legend = FALSE, arrow = ar, end_cap = ggraph::circle(.07, 'inches')) +
  ggraph::geom_node_point(color = "lightblue", size = 3) +
  ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
  ggplot2::theme_void()+
  ggplot2::theme(text = ggplot2::element_text(family = "Roboto Condensed"))


# Latent Dirichlet allocation (LDA) ---------------------------------------

# Every document is a mixture of topics
# Every topic is a mixture of words
# For this we need a document-term-matrix format

dtm_words <- df_words %>% 
  dplyr::count(id, word, sort = T) %>% 
  tidytext::cast_dtm(document = id, term = word, value=n)

lda <- topicmodels::LDA(dtm_words, k = 2, 
                        control = list(seed = 1234, verbose=1))
topics <- tidytext::tidy(lda, matrix = "beta")

topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(20, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta) %>%
  dplyr::mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ topic, scales = "free") +
  ggplot2::coord_flip() +
  tidytext::scale_x_reordered()

topics %>%
  dplyr::mutate(topic = paste0("topic", topic)) %>%
  tidyr::spread(topic, beta) %>%
  dplyr::filter_at(vars(starts_with("topic")), dplyr::any_vars(. > 0.001)) %>%
  dplyr::mutate(log_ratio = log2(topic2 / topic1))

documents <- tidytext::tidy(lda, matrix = "gamma")

documents %>%
  dplyr::group_by(document) %>%
  dplyr::top_n(1, gamma) %>%
  dplyr::ungroup()

assignments <- tidytext::augment(lda, data = dtm_words)
