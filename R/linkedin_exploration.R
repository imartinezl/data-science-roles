library(magrittr)

# https://ec.europa.eu/esco/portal/home

# https://www.onetonline.org/link/summary/15-1111.00

# http://www.skill-project.org/project

# https://www.reddit.com/r/dataisbeautiful/comments/25qjpz/how_many_employees_are_moving_between_companies_oc/


# PREPROCESS DATA ------------------------------------------------------

folder <- 'data/linkedin/1339943811/'
outfile <- "data/linkedin/data_linkedin.csv"
files <- list.files(folder, full.names = T)

process_row <- function(row){
  industry <- row$industry
  location <- row$location
  jobtitle <- row$positions[[1]]$title
  numconnections <- gsub("[^0-9.]", "", row$`num-connections`) %>% as.numeric()
  skills <- row$skills %>% 
    stringr::str_replace_all("[^[:alpha:][:space:]]","") %>% 
    stringr::str_squish() %>%
    paste(collapse = ";")
  
  industry <- ifelse(is.null(industry), NA, industry)
  location <- ifelse(is.null(location), NA, location)
  jobtitle <- ifelse(is.null(jobtitle), NA, jobtitle)
  numconnections <- ifelse(is.null(numconnections), NA, numconnections)
  skills <- ifelse(skills == "", NA, skills)
  
  row_data <- data.frame(industry, location, jobtitle, numconnections, skills, 
                         stringsAsFactors = F)
  return(row_data)
}
process_file <- function(infile){
  jsonlite::read_json(infile) %>% 
    lapply(process_row) %>% 
    dplyr::bind_rows() %>%
    na.omit()
}

if(!file.exists(outfile)){
  # dfs <- lapply(files[1:10], process_file)
  # dfs <- lapply(files, process_file)
  dfs <- pbapply::pblapply(files, process_file, cl = 4)
  dfs[lapply(dfs, nrow) > 0] %>% 
    dplyr::bind_rows() %>% 
    write.csv(outfile, row.names = F)
}


# READ & ANALYSIS ---------------------------------------------------------

outfile <- "data/linkedin/data_linkedin.csv"
df <- read.csv(outfile, stringsAsFactors = F)


# INDUSTRY ----------------------------------------------------------

df %>% 
  dplyr::select(industry) %>% 
  dplyr::group_by(industry) %>% 
  dplyr::count(industry) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(lng = cld3::detect_language(industry),
                lng_en = lng == "en") %>%
  dplyr::ungroup() %>% 
  # dplyr::filter(lng_en) %>%
  dplyr::top_n(50, wt = n) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(industry,n), y=n), width = 1)+
  ggplot2::coord_flip()+
  ggplot2::labs(x="Industry", y="Count")

industry_whitelist <- c("Information Technology and Services", "Computer Software", "Telecommunications", 
                        "Financial Services", "Banking", "Accounting", "Management Consulting",
                        "Higher Education", "Research", "Mechanical or Industrial Engineering",
                        "Architecture & Planning", "Electrical/Electronic Manufacturing", 
                        "Internet", "Computer Networking")


# SKILLS ------------------------------------------------------------------

# skills count on interesting industries
df %>% 
  dplyr::mutate(industry_interest = industry %in% industry_whitelist) %>% 
  dplyr::filter(industry_interest) %>% 
  dplyr::mutate(skills_sep = strsplit(skills, ';')) %>% 
  dplyr::select(skills_sep) %>% 
  tidyr::unnest(skills_sep) %>% 
  dplyr::group_by(skills_sep) %>% 
  dplyr::count(name="count") %>% 
  dplyr::ungroup() %>% 
  dplyr::top_n(n = 50, wt = count) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(skills_sep, count), y=count), width = 1)+
  ggplot2::coord_flip()+
  ggplot2::labs(x="Skills", y="Count")

# skills count separated by industries
df %>% 
  dplyr::mutate(industry_interest = industry %in% industry_whitelist) %>% 
  dplyr::filter(industry_interest) %>% 
  dplyr::mutate(skills_sep = strsplit(skills, ';')) %>% 
  dplyr::select(skills_sep, industry) %>% 
  tidyr::unnest(skills_sep) %>% 
  dplyr::group_by(skills_sep, industry) %>% 
  dplyr::count(name = "count") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(industry) %>% 
  dplyr::top_n(n = 15, wt=count) %>%
  dplyr::ungroup() %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(skills_sep, count), y=count), width = 1)+
  ggplot2::facet_wrap(industry~., scales = "free")+
  ggplot2::coord_flip()+
  ggplot2::labs(x="Skills", y="Count")

# skills wordcloud on one(multiple) specific industry(ies)
df %>% 
  dplyr::mutate(industry_interest = industry %in% c("Computer Software")) %>% 
  dplyr::filter(industry_interest) %>% 
  dplyr::mutate(skills_sep = strsplit(skills, ';')) %>% 
  dplyr::select(skills_sep) %>%
  tidyr::unnest(skills_sep) %>% 
  dplyr::group_by(skills_sep) %>% 
  dplyr::count(name = "count") %>% 
  dplyr::ungroup() %>% 
  dplyr::top_n(n = 250, wt = count) %>%
  ggplot2::ggplot()+
  ggwordcloud::geom_text_wordcloud(ggplot2::aes(label=skills_sep, size=count))+
  ggplot2::scale_size_area(max_size = 20)+
  ggplot2::theme_minimal()

# skills wordcloud separated by industry

industry_whitelist <- c("Computer Software", "Computer Networking", 
                        "Research", "Information Technology and Services")

df %>% 
  dplyr::mutate(industry_interest = industry %in% industry_whitelist) %>% 
  dplyr::filter(industry_interest) %>% 
  dplyr::mutate(skills_sep = strsplit(skills, ';')) %>% 
  dplyr::select(skills_sep, industry) %>% 
  tidyr::unnest(skills_sep) %>% 
  dplyr::group_by(skills_sep, industry) %>% 
  dplyr::count(name = "count") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(industry) %>% 
  dplyr::mutate(count = count / max(count)) %>% 
  dplyr::top_n(n = 50, wt=count) %>%
  dplyr::ungroup() %>% 
  ggplot2::ggplot()+
  ggwordcloud::geom_text_wordcloud_area(ggplot2::aes(label=skills_sep, size=count))+
  ggplot2::scale_size(range=c(3,8))+
  # ggplot2::scale_size_area(max_size = 10)+
  ggplot2::facet_wrap(.~industry)+
  ggplot2::theme_minimal()



# CONNECTIONS VS SKILLS ---------------------------------------------------

# number of skills vs number of connections (per industry)

p <- df %>% 
  dplyr::mutate(skills_count = stringr::str_count(skills, ';')) %>% 
  dplyr::select(skills_count, numconnections) %>%
  ggplot2::ggplot(ggplot2::aes(x=numconnections, y=skills_count))+
  # ggplot2::geom_point(alpha=0.1)+
  ggplot2::geom_jitter(width = 5, height = 5, alpha=0.05)
ggExtra::ggMarginal(p)

df %>% 
  dplyr::mutate(
    skills_count = stringr::str_count(skills, ';'),
    numconnections_dcr = cut(numconnections, breaks = seq(0, 500, by = 50 )),
  ) %>% 
  na.omit() %>% 
  dplyr::group_by(numconnections_dcr) %>% 
  dplyr::mutate(skills_count_mean = mean(skills_count),
                skills_count_median = median(skills_count)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(ggplot2::aes(x=skills_count, y=numconnections_dcr))+
  ggridges::geom_density_ridges(
    quantile_lines = T, quantiles = c(0.50, 0.9, 0.975), 
    scale = 0.9, alpha = 0.7, 
    vline_size = 1, vline_color = "red",
    position = ggridges::position_raincloud(adjust_vlines = TRUE)
  ) + 
  ggridges::theme_ridges()


# JOBTITLE ----------------------------------------------------------------

industry_whitelist <- c("Computer Software", "Computer Networking", 
                        "Research", "Information Technology and Services")
industry_whitelist <- c("Information Technology and Services")

# number of words in jobtitle
df %>% 
  dplyr::pull(jobtitle) %>% 
  stringr::str_count(pattern = "\\w+") %>% 
  hist()

df %>% 
  dplyr::filter(industry %in% industry_whitelist) %>%
  dplyr::mutate(nwords = stringr::str_count(jobtitle, pattern = "\\w+")) %>% 
  dplyr::filter(nwords > 0, nwords < 6) %>% 
  dplyr::count(nwords, jobtitle) %>% 
  dplyr::group_by(nwords) %>% 
  dplyr::arrange(-n) %>% 
  dplyr::slice( seq(1,c(30,15,15,10,10)[dplyr::cur_group_id()])) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot()+
  ggwordcloud::geom_text_wordcloud_area(ggplot2::aes(label=jobtitle, size=n))+
  ggplot2::scale_size(range=c(3,8))+
  # ggplot2::scale_size_area(max_size = 10)+
  ggplot2::facet_wrap(.~nwords)+
  ggplot2::theme_minimal()


# jobtitle filters: stopwords, stemming, etc

extrawords <- "
dev, scrummaster, frontend, ph.d, phd, ceo, cto, cco, 
sr, phd, gis, microsoft, sharepoint, ict, erp, cto, 
emea, seo, sales, presales, ibm, crm, analytics, 
research, designer, product, solution, solutions, manager, delivery
" %>%
  stringr::str_split(',') %>%
  unlist() %>% 
  stringr::str_trim() %>% 
  unique()
dictionary <- hunspell::dictionary(lang="en_US", add_words = extrawords)

conversions_jobtitle <- list(
  from = c(
    "sr", "pm", "bpm", "hr",
    "ceo", "cto", "coo",
    "cfo", "cco", "crm",
    "abap", "de", "qa", "dba",
    "bi", "erp", "ict",
    "pmo", "gis", "tech", "seo",
    "desk", "gerente", "emea", "i.t", "it",
    "fico", "ax", "mm", "manger", "asst", "techno", "db"
  ),
  to   = c(
    "senior", "project manager", "business process manager", "human resources",
    "chief executive officer", "chief technology officer", "chief operations officer", 
    "chief finance officer", "chief compliance officer", "customer relationship management",
    "advanced business application programming", "", "quality analyst", "database administrator",
    "business intelligence", "enterprise resource planning", "information communications technologies",
    "project management office", "geographic information system", "technology", "search engine optimization",
    "desktop", "manager", "europe middle east africa", "information technology", "information technology", 
    "finance controlling", "", "marketing manager", "manager", "assistant", "technology", "database"
  )
)
conversions_jobtitle$from <- paste("^", conversions_jobtitle$from, "$", sep="")



df %>% 
  dplyr::filter(industry %in% industry_whitelist) %>%
  dplyr::select(jobtitle) %>% 
  # dplyr::slice(1:3000) %>%
  dplyr::mutate(id = dplyr::row_number()) %>% 
  tidytext::unnest_tokens(output=unigrams, input=jobtitle, token="words", to_lower = T, drop = T) %>% 
  dplyr::filter(!unigrams %in% tidytext::stop_words$word) %>% 
  dplyr::mutate(unigrams_tmp = hunspell::hunspell_stem(unigrams, dictionary),
                valid = hunspell::hunspell_check(unigrams, dictionary)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(unigrams_tmp = dplyr::first(unigrams_tmp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(is_bad = is.na(unigrams_tmp),
                unigrams_tmp2 = stringi::stri_replace_all_regex(unigrams_tmp, 
                                                                conversions_jobtitle$from, 
                                                                conversions_jobtitle$to, 
                                                                vectorize_all = F),
                z=SnowballC::wordStem(unigrams, language = "english")) %>% View


# SKILLS VS JOBTITLE ------------------------------------------------------

d <- df %>% 
  # filter industry
  dplyr::filter(industry %in% industry_whitelist) %>%
  dplyr::select(jobtitle, skills) %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  # separate into unigrams
  tidytext::unnest_tokens(output=jobtitle_unigrams, input=jobtitle, token="words", to_lower = T, drop = T) %>%
  # filter stopwords
  dplyr::filter(!jobtitle_unigrams %in% tidytext::stop_words$word) %>% 
  # stem words
  dplyr::mutate(jobtitle_unigrams_tmp = hunspell::hunspell_stem(jobtitle_unigrams, dictionary)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(jobtitle_unigrams_tmp = dplyr::first(jobtitle_unigrams_tmp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(jobtitle_unigrams_tmp)) %>% 
  # remove numbers
  dplyr::mutate(jobtitle_unigrams_tmp = stringr::str_remove_all(jobtitle_unigrams_tmp, '\\w*[0-9]+\\w*\\s*')) %>% 
  # manual translation
  dplyr::mutate(jobtitle_unigrams_tmp = stringi::stri_replace_all_regex(jobtitle_unigrams_tmp, 
                                                               conversions_jobtitle$from, 
                                                               conversions_jobtitle$to, 
                                                               vectorize_all = F)) %>% 
  # collapse jobtitle
  dplyr::group_by(id, skills) %>% 
  dplyr::summarise(jobtitle = paste0(jobtitle_unigrams_tmp, collapse = " ")) %>% 
  dplyr::ungroup() %>% 
  # analyze number of words in jobtitle
  dplyr::mutate(jobtitle_nwords = stringr::str_count(jobtitle, pattern = "\\w+")) %>% 
  dplyr::add_count(jobtitle_nwords, sort=F, name="jobtitle_nwords_count") %>% 
  dplyr::mutate(jobtitle_nwords_count_rel = jobtitle_nwords_count/max(jobtitle_nwords_count)) %>% 
  # dplyr::count(jobtitle_nwords) %>% cumsum()
  # dplyr::pull(jobtitle_nwords) %>% hist(breaks=50)
  # separate jobtitle by bigrams and skills by ;
  # tidytext::unnest_tokens(output=jobtitle, input=jobtitle, token= "ngrams", n=2, to_lower = T, drop = T) %>%
  tidytext::unnest_tokens(output=jobtitle_ngrams, input=jobtitle, token= "skip_ngrams", n_min=2, n=4, k=2, to_lower = T, drop = F) %>%  
  # dplyr::count(jobtitle) %>% 
  dplyr::add_count(jobtitle_ngrams, sort=F, name="jobtitle_ngrams_count") %>% 
  dplyr::mutate(jobtitle_ngrams_count_rel = jobtitle_ngrams_count/max(jobtitle_ngrams_count),
                jobtitle_ngrams_nwords = stringr::str_count(jobtitle_ngrams, pattern = "\\w+"),
                jobtitle_ngrams_nwords_ratio = jobtitle_ngrams_nwords/jobtitle_nwords) %>% 
  dplyr::add_count(jobtitle_ngrams_nwords, sort=F, name="jobtitle_ngrams_nwords_count") %>% 
  dplyr::mutate(jobtitle_ngrams_nwords_count_rel = jobtitle_ngrams_nwords_count/max(jobtitle_ngrams_nwords_count)) %>% 
  dplyr::select(skills,
                jobtitle, # original jobtitle
                jobtitle_ngrams, # ngrams jobtitle
                jobtitle_nwords_count_rel, # distribution of (number of words) in original jobtitle
                jobtitle_ngrams_count_rel, # distribution of each ngram among all the ngrams
                jobtitle_ngrams_nwords_ratio, # ratio between (number of words) in ngram and the (number of words) in original jobtitle
                jobtitle_ngrams_nwords_count_rel, # distribution of (number of words) in ngrams jobtitle
  ) %>% 
  dplyr::mutate(jobtitle_weight = (jobtitle_nwords_count_rel*jobtitle_ngrams_count_rel*jobtitle_ngrams_nwords_ratio)^(1/3)) # %>% 
# give more weight to: 
# - jobtitles with more frequent number of words, (YES)
# - ngrams that are more popular (YES)
# - ngrams that are more reprentative of the original jobtitle (IMPORTANT)
# - ngrams with more frequent number of words (NAH)
d %>% 
  dplyr::pull(jobtitle_weight) %>% sort() %>% plot %>% abline(h=seq(0,1,by=0.05), col='red')


conversions_skills <- list(
  from = c(
    "soa", "ms", "itil",
    "iis", "wcf", "crm",
    "oop", "pmo", "db", "hr",
    "svn", "abap", "jms", 
    "ccnp", "pmi",
    "mcse", "bi", "sd",
    "adf", "biztalk", "cvs",
    "rup", "cmmi", "tdd",
    "bpo", "eai", "edi",
    "csm", "ooad", "sem",
    "seo", "sla", "vdi"
  ),
  to   = c(
    "service oriented architecture", "microsoft office", "information technology infrastructure library",
    "internet information services", "web component framework", "customer relationship management",
    "object oriented programming", "project management office", "database", "human resources",
    "subversion", "advanced business application programming", "java message service",
    "cisco certified network professional", "performance monitoring infrastructure",
    "microsoft certified systems engineer", "business intelligence", "software development",
    "automatic direction finder", "business talk", "concurrent versions system",
    "rational unified process", "capability maturity model integration", "test driven development",
    "business process outsourcing", "enterprise application integration", "enterprise data integration",
    "customer service management", "object oriented analysis design", "search engine marketing",
    "search engine optimization", "software license agreement", "virtual desktop infrastructure"
  )
)
conversions_skills$from <- paste("^", conversions_skills$from, "$", sep="")

d %>% 
  # filter less important jobs
  dplyr::filter(jobtitle_weight > 0.25) %>%
  dplyr::mutate(jobtitle_graph = jobtitle_ngrams) %>% 
  # separate skills by ";"
  tidytext::unnest_tokens(output=skills, input=skills, token= stringr::str_split, pattern = ";", to_lower=T, drop=T) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  # separate skills into unigrams
  tidytext::unnest_tokens(output=skills_unigrams, input=skills, token="words", to_lower = T, drop = T) %>%
  # filter stopwords
  dplyr::filter(!skills_unigrams %in% tidytext::stop_words$word) %>% 
  # stem words (NOT FOR SKILLS!!! TOO MANY TECHNOLOGIES ARE REMOVED)
  # dplyr::mutate(skills_unigrams_tmp = hunspell::hunspell_stem(skills_unigrams, dictionary_skills)) %>% 
  # dplyr::rowwise() %>%
  # dplyr::mutate(skills_unigrams_tmp = dplyr::first(skills_unigrams_tmp)) %>% 
  # dplyr::ungroup() %>% 
  # dplyr::filter(is.na(skills_unigrams_tmp)) %>% 
  # remove numbers (NOT FOR SKILLS!!! THERE ARE NOT ANY NUMBERS)
  # dplyr::mutate(skills_unigrams_tmp = stringr::str_remove_all(skills_unigrams, '\\w*[0-9]+\\w*\\s*')) %>% 
  # manual translation
  dplyr::mutate(skills_unigrams_tmp = stringi::stri_replace_all_regex(skills_unigrams, 
                                                               conversions_skills$from, 
                                                               conversions_skills$to, 
                                                               vectorize_all = F)) %>% 
  # join back the skills unigrams
  dplyr::group_by(id, jobtitle_graph) %>% 
  dplyr::summarise(skills = paste0(skills_unigrams_tmp, collapse = " ")) %>% 
  dplyr::ungroup() %>% 
  dplyr::count(jobtitle_graph, skills, name="count_pair", sort=T) %>% 
  # tidytext::bind_tf_idf(term = skills, document = jobtitle, count_pair) %>%
  # widyr::pairwise_count(skills, jobtitle) %>%
  # widyr::pairwise_cor(skills, jobtitle, sort = T, upper=F) %>% 
  dplyr::filter(jobtitle_graph != skills) %>% 
  dplyr::group_by(jobtitle_graph) %>% 
  dplyr::top_n(n = 1, wt=count_pair) %>% 
  dplyr::ungroup() %>% 
  tidygraph::as_tbl_graph() %>% 
  tidygraph::activate(nodes) %>% 
  tidygraph::mutate(
    source = tidygraph::node_is_source(),
    # count = merge(., data.frame(node=c(d$jobtitle, d$skills)) %>%
    #                 dplyr::filter(node %in% name) %>%
    #                 dplyr::count(node), by.x="name", by.y="node", sort=F) %>%
    #   dplyr::pull(n)
  ) %>% 
  # tidygraph::activate(edges) %>% 
  # ggraph::ggraph(layout = "fr") +
  ggraph::ggraph(layout = "grid") +
  # ggraph::geom_edge_bend(ggplot2::aes(edge_width=count_pair),
  #                        strength = 0.5, show.legend = F, edge_color="#000000",
  #                        start_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches'),
  #                        end_cap = ggraph::rectangle(width = 0.6, height = 0.2, 'inches')) +
  ggraph::geom_node_label(ggplot2::aes(label = stringr::str_wrap(name, width=10), color=source), 
                          family="Roboto Medium", lineheight = .8, #size=4, 
                          repel = F, show.legend = F) +
  ggraph::scale_edge_width_continuous(range=c(0.1,2))+
  # ggplot2::scale_size_continuous(range = c(3, 5))+
  # ggplot2::scale_alpha_continuous(range = c(0.5, 2.0))+
  ggplot2::scale_color_manual(values=c("#8338ec","#ff006e"))+
  ggplot2::theme_void()
