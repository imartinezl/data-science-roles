library(magrittr)
library(keras)

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
  dplyr::mutate(id = 1:dplyr::n()) %>% 
  # dplyr::slice(1:10)
  dplyr::sample_n(50000)




# clean job title and description

# job description
x_corpus <- tm::VCorpus(tm::VectorSource(df_subset$jobDescription))
##Removing HTML
x_corpus <- tm::tm_map(x_corpus, tm::content_transformer(textclean::replace_html))
##Removing Punctuation
x_corpus <- tm::tm_map(x_corpus, tm::content_transformer(tm::removePunctuation), preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)
##Removing numbers
x_corpus <- tm::tm_map(x_corpus, tm::removeNumbers)
##Converting to lower case
x_corpus <- tm::tm_map(x_corpus, tm::content_transformer(tolower))
##Removing stop words
x_corpus <- tm::tm_map(x_corpus, tm::content_transformer(tm::removeWords), tm::stopwords("english"))
##Stemming
# x_corpus <- tm::tm_map(x_corpus, tm::stemDocument) # maybe not
##Whitespace
x_corpus <- tm::tm_map(x_corpus, tm::stripWhitespace)
# Create Document Term Matrix
# x_dtm <- tm::DocumentTermMatrix(x_corpus)
# 
# x_corpus_sparse <- tm::removeSparseTerms(x_dtm, 0.9)
# x_dtm_matrix <- as.matrix(x_corpus_sparse)
# dim(x_dtm_matrix)

x_text <- data.frame(text=unlist(sapply(x_corpus, '[', "content")), stringsAsFactors = F)

x_num_words <- 2000
x_max_length <- 25
x_text_vectorization <- keras::layer_text_vectorization(
  standardize = NULL,
  max_tokens = x_num_words, 
  output_sequence_length = x_max_length, 
  ngrams = 1
)

x_text_vectorization %>% 
  keras::adapt(x_text$text)
keras::get_vocabulary(x_text_vectorization) %>% head(100)

x_text_vectorization(matrix(x_text$text[1], ncol = 1))

# job title

y_corpus <- tm::VCorpus(tm::VectorSource(df_subset$jobTitle))
##Removing Punctuation
y_corpus <- tm::tm_map(y_corpus, tm::content_transformer(tm::removePunctuation), preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)
##Removing numbers
y_corpus <- tm::tm_map(y_corpus, tm::removeNumbers)
##Converting to lower case
y_corpus <- tm::tm_map(y_corpus, tm::content_transformer(tolower))
##Removing stop words
y_corpus <- tm::tm_map(y_corpus, tm::content_transformer(tm::removeWords), tm::stopwords("english"))
##Stemming
# y_corpus <- tm::tm_map(y_corpus, tm::stemDocument) # maybe not
##Whitespace
y_corpus <- tm::tm_map(y_corpus, tm::stripWhitespace)
# Create Document Term Matrix
# y_dtm <- tm::DocumentTermMatrix(y_corpus)
# 
# y_corpus_sparse <- tm::removeSparseTerms(y_dtm, 0.9)
# y_dtm_matrix <- as.matrix(y_corpus_sparse)
# dim(y_dtm_matrix)

y_text <- data.frame(text=unlist(sapply(y_corpus, '[', "content")), stringsAsFactors = F)

# categorize job title to job label
job_label <- c("manager", "project|manager", "software|engineer", "business|analyst", "data|analyst", "data|scientist",
               "big|data|engineer", "machine|learning|engineer", "automation|engineer", "data|engineer",
               "support|engineer", "devops|engineer", "database|administrator", "financial|analyst")

y_train <- y_text %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(job_label_any = sum(stringr::str_count(text, job_label)) > 0,
                job_label_idx = ifelse(job_label_any, which.max(stringr::str_count(text, job_label)), 0),
                job_label_text = ifelse(is.na(job_label_idx), NA, job_label[job_label_idx])) %>% 
  # dplyr::filter(!is.na(job_label_idx)) %>% 
  dplyr::pull(job_label_idx) %>% 
  keras::to_categorical()

# alternative 1:
y_num_words <- 100
# y_max_length <- 30
y_text_vectorization <- keras::layer_text_vectorization(
  max_tokens = y_num_words,
  # output_sequence_length = y_max_length,
  output_mode = "binary",
  ngrams = 2
)

y_text_vectorization %>% 
  keras::adapt(y_text$text)
y_vocabulary <- keras::get_vocabulary(y_text_vectorization)
y_vocabulary

y_text_vectorization(matrix(y_text$text[1], ncol = 1))
y_train <- as.matrix(y_text_vectorization(matrix(y_text$text, ncol = 1)))


# MODEL
input <- keras::layer_input(shape = c(1), dtype = "string")
output <- input %>% 
  x_text_vectorization() %>% 
  keras::layer_embedding(input_dim = x_num_words+1, output_dim = 32) %>%
  keras::layer_global_average_pooling_1d() %>%
  keras::layer_dense(units = 32, activation = "relu") %>%
  keras::layer_dropout(0.2) %>%
  keras::layer_dense(units = 64, activation = "relu") %>%
  keras::layer_dropout(0.2) %>%
  keras::layer_dense(units = ncol(y_train), activation = "sigmoid")

model <- keras::keras_model(input, output)

model %>% keras::compile(
  optimizer = 'adam',
  # loss = 'mse', #categorical_crossentropy',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)
summary(model)
history <- model %>% keras::fit(
  x = x_text$text,
  y = y_train,
  batch_size = 10,
  epochs = 100,
  verbose = 1,
  validation_split = 0.1
)

results <- model %>% keras::evaluate(x_text$text, y_train, verbose = 1, batch_size=10)
results
plot(history)

y_pred <- model %>% 
  keras::predict_on_batch(x_text$text) %>% round() %>% 
  apply(1, function(x) paste(y_vocabulary[which(x==1)-1], collapse = ", "))

i <- sample(1:50000, 1)
y_text[i,1]
x_vocabulary <- keras::get_vocabulary(x_text_vectorization)
x_vocabulary[as.matrix(x_text_vectorization(matrix(x_text$text[i], ncol = 1)))]


y_pred_i <- model %>% 
  keras::predict_on_batch(x_text$text[i])
  # round() %>% 
  # apply(1, function(x) paste(y_vocabulary[which(x==1)-1], collapse = ", "))

n_top <- 15
y_pred_index <- order(y_pred_i, decreasing = T)-1
y_pred_index <- ifelse(y_pred_index==0, NA, y_pred_index)
data.frame(word=y_vocabulary[y_pred_index][1:n_top], 
           amount=sort(y_pred_i, decreasing = T)[1:n_top])

