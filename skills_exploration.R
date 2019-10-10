
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


extract_skills <- function(t){
  t %>% stringr::str_replace_all("\\[|\\]|'", "") %>% stringr::str_split(',') %>% unlist %>% stringr::str_trim() %>% list()
}
mystopwords <- tibble(word = c("\n"))

df_skill <- df %>% 
  # dplyr::slice(1:200) %>%
  dplyr::filter(Skill != "") %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(word = extract_skills(Skill)) %>% 
  dplyr::ungroup() %>% 
  tidyr::unnest(word) %>% 
  dplyr::select(id, Job_Type, word) %>% 
  dplyr::anti_join(tidytext::stop_words, mystopwords, by="word")

df_skill %>%
  dplyr::count(word, sort = TRUE) %>% View

set.seed(123)
df_skill %>% 
  widyr::pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  dplyr::filter(n >= 300) %>%
  igraph::graph_from_data_frame() %>%
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link(ggplot2::aes(edge_alpha = scales::rescale(n, c(0.1,1)), 
                                      edge_width = scales::rescale(n, c(1,5))),
                                      edge_colour = "royalblue") +
  ggraph::geom_node_point(size = 5) +
  ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                         point.padding = grid::unit(0.2, "lines")) +
  ggplot2::theme_void()

set.seed(1)
df_skill %>% 
  dplyr::group_by(word) %>%
  dplyr::filter(n() >= 50) %>%
  widyr::pairwise_cor(word, id, sort = TRUE, upper = FALSE) %>%
  dplyr::filter(abs(correlation) > 0.2) %>%
  igraph::graph_from_data_frame() %>%
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link(ggplot2::aes(edge_alpha = scales::rescale(correlation, c(0.1,1)), 
                                      edge_width = scales::rescale(correlation, c(0.5,2))),
                         edge_colour = "royalblue") +
  ggraph::geom_node_point(size = 5) +
  ggraph::geom_node_label(ggplot2::aes(label = name), repel = TRUE,
                         point.padding = grid::unit(0.2, "lines")) +
  ggplot2::theme_void()
