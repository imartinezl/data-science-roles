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
  dplyr::sample_n(1000)



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
x_dtm <- tm::DocumentTermMatrix(x_corpus)

x_corpus_sparse <- tm::removeSparseTerms(x_dtm, 0.9)
x_dtm_matrix <- as.matrix(x_corpus_sparse)
dim(x_dtm_matrix)

x_dtm_text <- data.frame(text=unlist(sapply(x_corpus, '[', "content")), stringsAsFactors=F)

# x_text <- x_dtm_text$text
# max_features <- 2000
# x_tokenizer <- keras::text_tokenizer(num_words = max_features)
# x_tokenizer %>% 
#   keras::fit_text_tokenizer(x_text)
# 
# x_text_seqs <- keras::texts_to_sequences(x_tokenizer, x_text)
# summary(sapply(x_text_seqs, length))
# 
# x_maxlen <- 500
# x_train <- x_text_seqs %>%
#   keras::pad_sequences(maxlen = x_maxlen)
# dim(x_train)

num_words <- 10000
max_length <- 50
text_vectorization <- keras::layer_text_vectorization(
  max_tokens = num_words, 
  output_sequence_length = max_length, 
)

x_text <- df_subset$jobDescription # x_dtm_text$text
text_vectorization %>% 
  keras::adapt(x_text)
keras::get_vocabulary(text_vectorization)

text_vectorization(matrix(x_text[1], ncol = 1))

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
y_dtm <- tm::DocumentTermMatrix(y_corpus)

y_corpus_sparse <- tm::removeSparseTerms(y_dtm, 0.9)
y_dtm_matrix <- as.matrix(y_corpus_sparse)
dim(y_dtm_matrix)

y_dtm_text <- data.frame(text=unlist(sapply(y_corpus, '[', "content")), stringsAsFactors=F)

# cleaning
job_label <- c("manager", "project|manager", "software|engineer", "business|analyst", "data|analyst", "data|scientist",
               "big|data|engineer", "machine|learning|engineer", "automation|engineer", "data|engineer",
               "support|engineer", "devops|engineer", "database|administrator", "financial|analyst")

y_train <- y_dtm_text %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(job_label_any = sum(stringr::str_count(text, job_label)) > 0,
                job_label_idx = ifelse(job_label_any, which.max(stringr::str_count(text, job_label)), 0),
                job_label_text = ifelse(is.na(job_label_idx), NA, job_label[job_label_idx])) %>% 
  # dplyr::filter(!is.na(job_label_idx)) %>% 
  dplyr::pull(job_label_idx) %>% 
  keras::to_categorical()



# MODEL 1
# embedding_dims <- 50
# filters <- 64
# kernel_size <- 3
# hidden_dims <- 100
# model <- keras::keras_model_sequential() %>% 
#   keras::layer_embedding(max_features, embedding_dims, input_length = x_maxlen) %>%
#   # keras::layer_dropout(0.2) %>%
#   # keras::layer_conv_1d(filters, kernel_size, padding = "valid", activation = "relu", strides = 1) %>%
#   keras::layer_global_max_pooling_1d() %>%
#   keras::layer_dense(units = hidden_dims, activation = "relu") %>%
#   # keras::layer_dropout(0.2) %>%
#   keras::layer_dense(ncol(y_train), activation = "softmax") %>%
#   keras::compile(
#     loss = "categorical_crossentropy",
#     optimizer = "adam",
#     metrics = "accuracy"
#   )
# 
# batch_size <- 10
# epochs <- 100
# hist <- model %>%
#   keras::fit(
#     x_train,
#     y_train,
#     batch_size = batch_size,
#     epochs = epochs,
#     validation_split = 0.3
#   )

# MODEL 2
input <- keras::layer_input(shape = c(1), dtype = "string")
output <- input %>% 
  text_vectorization() %>% 
  keras::layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  keras::layer_global_average_pooling_1d() %>%
  keras::layer_dense(units = 16, activation = "relu") %>%
  keras::layer_dropout(0.5) %>% 
  keras::layer_dense(units = 12, activation = "softmax")

model <- keras::keras_model(input, output)

model %>% keras::compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = list('accuracy')
)

history <- model %>% keras::fit(
  x = training$text,
  y = keras::to_categorical(training$tag),
  batch_size = 10,
  epochs = 10,
  verbose = 2,
  validation_split = 0.2
)

# sw <- c(tm::stopwords('en'), 
#         tidytext::stop_words %>% dplyr::pull(word),
#         stopwords::data_stopwords_smart %>% unlist(),
#         # rtweet::stopwordslangs %>% dplyr::filter(lang=="en", p>0.99) %>% dplyr::pull(word)
#         c("ii")
# ) %>% unique()
# stopwords_regex <- paste(sw, collapse = '\\b|\\b')
# stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
# 
# df_subset_clean <- df_subset %>% 
#   dplyr::select(jobTitle) %>% 
#   dplyr::mutate(jobTitle_1 = stringr::str_to_lower(jobTitle),
#                 # jobTitle_3 = stringr::str_remove_all(jobTitle_2, stopwords_regex),
#                 jobTitle_2 = textclean::replace_non_ascii(jobTitle_1),
#                 jobTitle_3 = tm::removePunctuation(jobTitle_2, preserve_intra_word_contractions=T, preserve_intra_word_dashes=T),
#                 jobTitle = jobTitle_3
#                 )

df_subset_clean %>% tidytext::unnest_tokens(word, jobDescription, drop = T) %>%
  dplyr::count(word, sort = TRUE) %>% View

# first approach
job_label <- c("manager", "project|manager", "software|engineer", "business|analyst", "data|analyst", "data|scientist",
                "big|data|engineer", "machine|learning|engineer", "automation|engineer", "data|engineer",
                "support|engineer", "devops|engineer", "database|administrator", "financial|analyst")


example <- df_subset$jobTitle[2]
stringr::str_count(example, job_label)
job_label_word_count <- stringi::stri_count_words(job_label)
count_relative <- stringr::str_count(example, job_label)/job_label_word_count
sum(count_relative) == 0
which.max(count_relative)

df_model <- df_subset_clean %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(job_label_any = sum(stringr::str_count(jobTitle, job_label)) > 0,
                job_label_idx = ifelse(job_label_any, which.max(stringr::str_count(jobTitle, job_label)), NA),
                job_label_text = ifelse(is.na(job_label_idx), NA, job_label[job_label_idx])) %>% 
  dplyr::filter(!is.na(job_label_idx)) %>% 
  dplyr::select(text = jobDescription, tag = job_label_idx)

ncells <- ncol(keras::to_categorical(df_model$tag))

training_id <- sample.int(nrow(df_model), size = nrow(df_model)*0.8)
training <- df_model[training_id,]
testing <- df_model[-training_id,]

df_model %>% dplyr::count(tag)

df_model$text %>% 
  strsplit(" ") %>% 
  sapply(length) %>% 
  summary()

num_words <- 10000
max_length <- 50
text_vectorization <- keras::layer_text_vectorization(
  max_tokens = num_words, 
  output_sequence_length = max_length, 
)

text_vectorization %>% 
  keras::adapt(df_model$text)
keras::get_vocabulary(text_vectorization)

text_vectorization(matrix(df_model$text[1], ncol = 1))

input <- keras::layer_input(shape = c(1), dtype = "string")
output <- input %>% 
  text_vectorization() %>% 
  keras::layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  keras::layer_global_average_pooling_1d() %>%
  keras::layer_dense(units = 16, activation = "relu") %>%
  keras::layer_dropout(0.5) %>% 
  keras::layer_dense(units = 12, activation = "softmax")

model <- keras::keras_model(input, output)

model %>% keras::compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = list('accuracy')
)

history <- model %>% keras::fit(
  x = training$text,
  y = keras::to_categorical(training$tag),
  batch_size = 10,
  epochs = 10,
  verbose = 2,
  validation_split = 0.2
)

results <- model %>% keras::evaluate(testing$text, testing$tag, verbose = 0)
results
plot(history)



# example

reuters <- keras::dataset_reuters(num_words = 1000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters
train_data[[1]]

word_index <- keras::dataset_reuters_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index
decoded_newswire <- sapply(train_data[[1]], function(index) {
  # Note that our indices were offset by 3 because 0, 1, and 2
  # are reserved indices for "padding", "start of sequence", and "unknown".
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
})


vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}
x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)


to_one_hot <- function(labels, dimension = 46) {
  results <- matrix(0, nrow = length(labels), ncol = dimension)
  for (i in 1:length(labels))
    results[i, labels[[i]]] <- 1
  results
}
one_hot_train_labels <- to_one_hot(train_labels)
one_hot_test_labels <- to_one_hot(test_labels)


model <- keras::keras_model_sequential() %>% 
  keras::layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>% 
  keras::layer_dense(units = 64, activation = "relu") %>% 
  keras::layer_dense(units = 46, activation = "softmax")

model %>% keras::compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% keras::fit(
  x_train,
  one_hot_train_labels,
  epochs = 20,
  batch_size = 512,
  validation_split = 0.2
)
