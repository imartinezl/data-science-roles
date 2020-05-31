library(magrittr)

file_path <- "data/glassdoor.csv.gz"
df <- data.table::fread(file_path)

language_path <- "data/glassdoor-language.rds"
if(file.exists(language_path)){
  language <- readRDS(language_path)
  jobdescription_lang <- language[[1]]
  jobtitle_lang <- language[[2]]
}else{
  jobdescription_lang <- cld3::detect_language(df %>% dplyr::pull(job.description))
  jobtitle_lang <- cld3::detect_language(df %>% dplyr::pull(header.jobTitle))
  saveRDS(list(jobdescription_lang, jobtitle_lang), language_path)
}

df_subset <- df %>% 
  dplyr::select(jobTitle = header.jobTitle,
                jobDescription = job.description) %>% 
  dplyr::mutate(jobtitle_lang = jobtitle_lang,  
                jobdescription_lang = jobdescription_lang) %>% 
  dplyr::filter(jobdescription_lang == "en", ) %>% 
  dplyr::mutate(id = 1:dplyr::n())
# dplyr::slice(1:1000)

df_subset %>% 
  tidytext::unnest_tokens(word, jobTitle, drop=F) %>% 
  dplyr::anti_join(tidytext::stop_words) %>%
  dplyr::count(word, sort = TRUE) 

df_subset %>% 
  dplyr::filter(grepl("projectmanager",jobTitle, ignore.case = T)) %>% View

# tf-idf ------------------------------------------------------------------
# not really interesting

words <- df_subset %>% 
  tidytext::unnest_tokens(word, jobTitle, drop=F) %>% 
  dplyr::anti_join(tidytext::stop_words) %>%
  dplyr::count(id, word, sort = TRUE) 

total_words <- words %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(total = sum(n))

words <- dplyr::left_join(words, total_words)

words %>%
  tidytext::bind_tf_idf(word, id, n) %>%
  dplyr::select(-total) %>%
  dplyr::arrange(desc(tf_idf)) %>% View


# n-grams -----------------------------------------------------------------

# bi-grams
df_subset %>%
  tidytext::unnest_tokens(bigram, jobTitle, token = "ngrams", n = 2, drop=F) %>% 
  tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::count(word1, word2, sort = TRUE) 

# tri-grams
df_subset %>%
  tidytext::unnest_tokens(trigram, jobTitle, token = "ngrams", n = 3, drop=F) %>% 
  tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::filter(!word3 %in% tidytext::stop_words$word) %>% 
  dplyr::count(word1, word2, word3, sort = TRUE)



# visualizing a network of bigrams ----------------------------------------

bigrams_count <- df_subset %>%
  tidytext::unnest_tokens(bigram, jobTitle, token = "ngrams", n = 2, drop=F) %>% 
  tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::count(word1, word2, sort = TRUE) 

ar <- grid::arrow(type = "closed", length = grid::unit(.10, "inches"), angle=30)
g <- bigrams_count %>% 
  dplyr::filter(n > 500) %>%
  na.omit() %>% 
  tidygraph::as_tbl_graph() %>% 
  tidygraph::mutate(centrality_degree = tidygraph::centrality_degree(mode = 'in')) 
seed <- 81 #sample(1:100, 1)
set.seed(seed)
g %>% 
  # ggraph::ggraph(layout = 'dh', weight.node.dist=1, weight.border = 1, maxiter=5, fineiter=5,
  #                weight.edge.lengths = 0, weight.edge.crossings = 2, weight.node.edge.dist=0)
  # ggraph::ggraph(layout = 'tree', mode="out")
  # ggraph::ggraph(layout = 'mds') 
  ggraph::ggraph(layout = 'fr')+
  ggraph::geom_edge_bend(ggplot2::aes(edge_alpha = scales::rescale(n, c(0.1,0.5)), 
                                      edge_width = scales::rescale(n, c(0.5,2))),
                         edge_colour = "black", arrow = ar, strength=0.5, 
                         start_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'),
                         end_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'))+
  ggraph::geom_node_text(ggplot2::aes(label = name, size=centrality_degree), family="Roboto",
                         repel = F, vjust = 0.5, hjust = 0.5, show.legend = F)+
  ggplot2::scale_size_continuous(range = c(3,4))+
  ggplot2::theme_void()

node_names <- igraph::V(g) %>% igraph::as_ids()
node_id <- sample(igraph::vcount(g), 1)
for(node_id in 1:igraph::vcount(g)){
  img_name <- paste0("bigrams_", node_id, "_", node_names[node_id], ".png")
  ego_order <- 1
  ego_size <- igraph::ego_size(g, nodes = node_id, mode = "out", order = ego_order)
  set.seed(81)
  g %>% 
    tidygraph::activate(nodes)  %>%
    tidygraph::mutate(source = dplyr::row_number() == node_id,
                      ego = name %in% igraph::as_ids(igraph::ego(g, nodes = node_id, mode = "out", order = ego_order)[[1]]),
                      type = as.factor(ifelse(source, 0, ifelse(ego, 1, 2))) ) %>%
    tidygraph::activate(edges) %>%
    tidygraph::mutate(neighbor = (tidygraph::.N()$ego[from] & tidygraph::.N()$ego[to])) %>%
    ggraph::ggraph(layout = 'fr')+
    ggraph::geom_edge_bend(ggplot2::aes(edge_alpha = scales::rescale(n, c(0.1,0.5)), 
                                        edge_width = scales::rescale(n, c(0.5,2))),
                           edge_colour = "grey", arrow = ar, strength=0.5, 
                           start_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'),
                           end_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'))+
    {if(ego_size>1)ggraph::geom_edge_bend(ggplot2::aes(filter=neighbor, 
                                                       edge_width = scales::rescale(n, c(0.5,2)) ), 
                                          edge_colour="red", arrow = ar, strength=1, 
                                          start_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'),
                                          end_cap = ggraph::rectangle(width = 0.5, height = 0.2, 'inches'))
    }+
    ggraph::geom_node_text(ggplot2::aes(label = name, size=centrality_degree, color=type), family="Roboto",
                           repel = F, vjust = 0.5, hjust = 0.5, show.legend = F)+
    ggplot2::scale_size_continuous(range = c(3,4))+
    ggplot2::scale_color_manual(values=c("red", "black", "grey"))+
    ggplot2::theme_void()+
    ggplot2::ggsave(img_name, dpi=100, width=7, height=7)
}


# Pairs of words -----------------------------------

# Pairwise correlation: how often they appear together relative to how often they appear separately
#  phi coefficient: how much more likely it is that either both word X and Y appear, or neither do, than that one appears without the other.
df_subset %>% 
  dplyr::select(id, jobTitle) %>% 
  tidytext::unnest_tokens(word, jobTitle, drop=F) %>% 
  dplyr::anti_join(tidytext::stop_words) %>% 
  widyr::pairwise_count(word, id, sort = T, upper=F) %>% head

words <- df_subset %>% 
  dplyr::select(id, jobTitle) %>% 
  tidytext::unnest_tokens(word, jobTitle, drop=F) %>% 
  dplyr::anti_join(tidytext::stop_words) %>% 
  dplyr::group_by(word) %>%
  dplyr::filter(dplyr::n() >= 200)

set.seed(81)
words %>% 
  widyr::pairwise_cor(word, id, sort = T, upper=F) %>% #dplyr::pull(correlation) %>% quantile(seq(0,1,by=0.1))
  dplyr::mutate(correlation_abs = abs(correlation)) %>% 
  tidygraph::filter(correlation > 0.08) %>% 
  tidygraph::as_tbl_graph() %>% 
  tidygraph::activate(nodes) %>% 
  tidygraph::mutate(n = words %>% dplyr::filter(word %in% name) %>% dplyr::group_by(word) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::pull(n)) %>% 
  tidygraph::activate(edges) %>% 
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_bend(strength = 0.5, show.legend = F,
                         start_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches'),
                         end_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches')) +
  ggraph::geom_node_text(ggplot2::aes(label = name, size=n), repel = F, show.legend = F) +
  ggplot2::scale_size_continuous(range = c(3,4))+
  ggplot2::scale_alpha_continuous(range = c(0.7, 1.0))+
  ggplot2::theme_void()



# Pairwise count of bigrams -----------------------------------------------

bigrams <- df_subset %>% 
  dplyr::select(id, jobTitle) %>% 
  tidytext::unnest_tokens(bigram, jobTitle, token = "ngrams", n = 2, drop=F) %>% 
  tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::group_by(bigram) %>%
  dplyr::filter(dplyr::n() >= 180)
  
bigrams %>% 
  widyr::pairwise_cor(bigram, id, sort = T, upper=F) %>% #dplyr::pull(correlation) %>% quantile(seq(0.98,1,0.001))
  dplyr::mutate(correlation_abs = abs(correlation)) %>% 
  tidygraph::filter(correlation > 0.08) %>% 
  tidygraph::as_tbl_graph() %>% 
  tidygraph::activate(nodes) %>% 
  tidygraph::mutate(n = bigrams %>% dplyr::filter(bigram %in% name) %>% dplyr::group_by(bigram) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::pull(n),
                    centrality_degree = tidygraph::centrality_degree(),
                    tmp = tidygraph::node_is_keyplayer(k = 15)) %>%  
  tidygraph::activate(edges) %>% 
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_bend(strength = 0.5, show.legend = F, edge_color="grey",
                         start_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches'),
                         end_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches')) +
  ggraph::geom_node_text(ggplot2::aes(label = stringr::str_wrap(name, width=10), 
                                      alpha=sqrt(centrality_degree),
                                      color=tmp), 
                         family="Roboto Medium", size=4, lineheight = .8, 
                         repel = F, show.legend = F) +
  ggplot2::scale_alpha_continuous(range = c(0.5, 2.0))+
  ggplot2::scale_color_manual(values=c("#8338ec","#ff006e"))+
  ggplot2::theme_void()

# Pairwise count of unigrams and bigrams -------------------------------------------

unigrams <- df_subset %>% 
  dplyr::select(id, jobTitle) %>% 
  tidytext::unnest_tokens(word, jobTitle, drop=F) %>% 
  dplyr::anti_join(tidytext::stop_words, ) %>% 
  dplyr::group_by(word) %>%
  dplyr::filter(dplyr::n() >= 200) %>% 
  dplyr::select(id, item=word)

bigrams <- df_subset %>% 
  dplyr::select(id, jobTitle) %>% 
  tidytext::unnest_tokens(bigram, jobTitle, token = "ngrams", n = 2, drop=F) %>% 
  tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::group_by(bigram) %>%
  dplyr::filter(dplyr::n() >= 200) %>% 
  dplyr::select(id, item=bigram)

trigrams <- df_subset %>%
  tidytext::unnest_tokens(trigram, jobTitle, token = "ngrams", n = 3, drop=F) %>% 
  tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ", remove=F) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words$word) %>% 
  dplyr::filter(!word3 %in% tidytext::stop_words$word) %>% 
  dplyr::group_by(trigram) %>%
  dplyr::filter(dplyr::n() >= 200) %>% 
  dplyr::select(id, item=trigram)

words <- list(bigrams, trigrams) %>% dplyr::bind_rows()


words %>% 
  widyr::pairwise_cor(item, id, sort = T, upper=F) %>% #dplyr::pull(correlation) %>% quantile(seq(0.98,1,0.001))
  dplyr::mutate(correlation_abs = abs(correlation)) %>% 
  tidygraph::filter(correlation > 0.10) %>% 
  tidygraph::as_tbl_graph() %>% 
  # tidygraph::activate(nodes) %>% 
  # tidygraph::mutate(n = bigrams %>% dplyr::filter(bigram %in% name) %>% dplyr::group_by(bigram) %>% dplyr::summarise(n=dplyr::n()) %>% dplyr::pull(n),
  #                   centrality_degree = tidygraph::centrality_degree(),
  #                   tmp = tidygraph::node_is_keyplayer(k = 15)) %>%  
  # tidygraph::activatse(edges) %>% 
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_bend(strength = 0.5, show.legend = F, edge_color="grey",
                         start_cap = ggraph::rectangle(width = 0.2, height = 0.1, 'inches'),
                         end_cap = ggraph::rectangle(width = 0.2, height = 0.1, 'inches')) +
  ggraph::geom_node_text(ggplot2::aes(label = stringr::str_wrap(name, width=10)), 
                         family="Roboto", size=3, lineheight = .8, 
                         repel = F, show.legend = F) +
  ggplot2::scale_alpha_continuous(range = c(0.5, 2.0))+
  ggplot2::scale_color_manual(values=c("#8338ec","#ff006e"))+
  ggplot2::theme_void()

# add summary of job title from graph
