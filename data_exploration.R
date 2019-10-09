
# LIBRARIES & IMPORT DATA -------------------------------------------------

library(dplyr)
setwd("~/Documents/data-science-roles")

file_path <- 'data/indeed_job_dataset.csv'
df <- read.csv(file = file_path, stringsAsFactors = F)


# DATA EXPLORATION --------------------------------------------------------

df %>% 
  dplyr::group_by(Job_Type) %>% 
  dplyr::summarise(n = n(),
                   mean_nskills = mean(No_of_Skills, na.rm=T),
                   mean_nreviews = mean(No_of_Reviews, na.rm=T))

plot_mean_count <- function(df, columns, group="Job_Type"){
  df %>% 
    tidyr::gather(key, value, columns) %>% 
    dplyr::group_by_(group, "key") %>% 
    dplyr::summarise(count = mean(value)) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_col(ggplot2::aes_string(x="key", y="count", fill=group), width=0.5, position="dodge")+
    ggplot2::coord_flip()
}
radar_mean_count <- function(df, columns, group="Job_Type"){
  ngroup <- df %>% select(group) %>% unique() %>% nrow()
  ncolumns <- df %>% 
    dplyr::select(columns) %>%
    tidyr::gather(key,value) %>% 
    dplyr::filter(value==1) %>%
    dplyr::group_by(key) %>% 
    dplyr::summarise(n = n())
  radar_p1 <- df %>% 
    tidyr::gather(key, value, columns) %>% 
    dplyr::group_by_(group, "key") %>% 
    dplyr::summarise(count = sum(value)) %>% 
    # merge(ncolumns) %>% 
    # dplyr::mutate(count = count/n) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_polygon(ggplot2::aes_string(x="key", y="count", col=group, group=group), size=2, fill=NA)+
    ggplot2::geom_point(ggplot2::aes_string(x="key", y="count", col=group, group=group), size=5)+
    ggplot2::scale_color_manual(values = viridis::viridis(ngroup))+
    ggplot2::coord_polar()+
    ggplot2::theme_minimal()+
    ggplot2::theme(text = ggplot2::element_text(family = "Roboto Condensed"),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.5, 'cm'),
                   legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 10),vjust=2, size=15),
                   axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size=15),
                   axis.text.y = ggplot2::element_blank())
  
  
  radar_p2 <- df %>% 
    tidyr::gather(key, value, columns) %>% 
    dplyr::group_by_(group, "key") %>% 
    dplyr::summarise(count = mean(value)) %>%
    ggplot2::ggplot()+
    ggplot2::geom_polygon(ggplot2::aes_string(x="key", y="count", col=group, group=group), size=1, fill=NA)+
    ggplot2::geom_point(ggplot2::aes_string(x="key", y="count", col=group, group=group), size=2)+
    ggplot2::facet_grid(reformulate(group))+
    ggplot2::scale_color_manual(guide=F,values = viridis::viridis(ngroup))+
    ggplot2::coord_polar()+
    ggplot2::theme_minimal()+
    ggplot2::theme(text = ggplot2::element_text(family = "Roboto Condensed"),
                   strip.text = ggplot2::element_text(size=14),
                   axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size=10),
                   axis.text.y = ggplot2::element_blank())
  
  gridExtra::grid.arrange(radar_p1,radar_p2,ncol=1,heights=c(2,1))
}

# Distribution of technologies over job_types
technologies <- c("python", "sql", "machine.learning", "r", "hadoop", "tableau", "sas", "spark", "java")
plot_mean_count(df, technologies)
radar_mean_count(df, technologies)

# Distribution of states over job_types
states <- c("CA","NY","VA","TX","MA","IL","WA","MD","DC","NC","Other_states")
plot_mean_count(df, states)
radar_mean_count(df, states)

# Distribution of Company Sector over job_types
company_sector <- c("Consulting.and.Business.Services","Internet.and.Software",
                    "Banks.and.Financial.Services","Health.Care","Insurance","Other_industries")
plot_mean_count(df, company_sector)



# Skills Extraction -------------------------------------------------------

extract_skills <- function(t){
  t %>% stringr::str_replace_all("\\[|\\]|'", "") %>% stringr::str_split(',') %>% unlist %>% stringr::str_trim() %>% list()
}
skills_ext <- df %>% 
  # dplyr::slice(1:200) %>% 
  dplyr::filter(Skill != "") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(skill = extract_skills(Skill),
                nskills = length(skill)) %>% 
  dplyr::ungroup() %>% 
  tidyr::unnest(skill) %>% 
  dplyr::select(X, Job_Type, skill, nskills)
skills_ext


# Skills Wordcloud ------------------------------------------------------------------

skills_freq <- skills_ext %>% 
  dplyr::group_by(skill) %>% 
  dplyr::summarise(freq = n(),
                   mean_nskills_per_job = mean(nskills)) %>% 
  dplyr::ungroup()
wordcloud::wordcloud(words = skills_freq$skill, freq = skills_freq$freq, max.words = 200,
                     random.order = F, random.color = F, scale = c(2,0.8), rot.per = 0,
                     colors = RColorBrewer::brewer.pal(9,"Reds")[3:9], min.freq = 10)

skills_freq %>% 
  ggplot2::ggplot()+
  ggrepel::geom_text_repel(ggplot2::aes(x=mean_nskills_per_job, y=freq, label=skill,
                                        size=freq, color=mean_nskills_per_job), segment.alpha = 0)+
  ggplot2::scale_color_gradient(low="green3", high="violetred", trans = "log10",
                                guide = ggplot2::guide_colourbar(direction = "horizontal", title.position ="top")) +
  ggplot2::scale_size_continuous(range = c(3, 10), guide = FALSE) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position=c(.99, .99),
                 legend.justification = c("right","top"),
                 panel.grid.major = ggplot2::element_line(colour = "whitesmoke"))

# Skills Frequency --------------------------------------------------------
skills_onehot <- skills_ext %>% 
  dplyr::mutate(value=1) %>% 
  tidyr::spread(skill, value, fill=0)

n <- nrow(skills_onehot)
skills_names <- colnames(skills_onehot)[4:ncol(skills_onehot)]

skills_onehot %>% 
  dplyr::select(skills_names) %>% 
  dplyr::summarise_all(sum) %>% 
  tidyr::gather(key,value) %>% 
  dplyr::arrange(-value) %>% 
  dplyr::mutate(value = value/n,
                id = dplyr::row_number(),
                group = floor(id / 50)) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(key, value),y=value), width=1)+
  ggplot2::labs(x=NULL,y=NULL)+
  ggplot2::scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  ggplot2::coord_flip()+
  ggplot2::facet_wrap(group~., scales="free_y")+
  ggplot2::theme(axis.text.y = ggplot2::element_text(size=8),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank())

skills_ext %>% 
  dplyr::group_by(Job_Type, skill) %>% 
  dplyr::summarise(freq = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(-freq) %>% 
  dplyr::top_n(100, freq) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(skill,freq), y=freq, fill=Job_Type), width=1)+
  ggplot2::labs(x=NULL,y=NULL)+
  ggplot2::coord_flip()+
  ggplot2::facet_grid(cols=vars(Job_Type), scales="free_y")+
  ggplot2::theme(axis.text.y = ggplot2::element_text(size=8),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),)


# Naive Bayes -------------------------------------------------------------

n <- 15
mostfreq_skills <- skills_ext %>% 
  dplyr::group_by(skill) %>% 
  dplyr::summarise(freq = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(-freq) %>% 
  dplyr::top_n(n, freq)

df_cluster <- skills_onehot %>% 
  dplyr::select(Job_Type, mostfreq_skills$skill) %>% 
  dplyr::mutate(Job_Type = as.factor(Job_Type))
factor(df$Job_Type) %>% levels()
df_cluster %>% unique() %>% nrow()


y <- "Job_Type"
set.seed(1)
trainIndex <- caret::createDataPartition(df_cluster[[y]], p=0.7)$Resample1
train <- df_cluster[trainIndex, ]
test <- df_cluster[-trainIndex, ]
model_summary <- function(model, y){
  trainPred <- predict(model, newdata = train, type = "class")
  trainTable <- table(train[[y]], trainPred)
  testPred <- predict(model, newdata=test, type="class")
  testTable <- table(test[[y]], testPred)
  trainAcc <- sum(diag(trainTable))/sum(trainTable)
  testAcc <- sum(diag(testTable))/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}

# Naive Bayes
f <- as.formula(paste(y,'~.'))
NB <- e1071::naiveBayes(f, data=train)
NB
model_summary(NB, y)

model_training <- function(n){
  mostfreq_skills <- skills_ext %>% 
    dplyr::group_by(skill) %>% 
    dplyr::summarise(freq = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(-freq) %>% 
    dplyr::top_n(n, freq)
  
  df_cluster <- skills_onehot %>% 
    dplyr::select(Job_Type, mostfreq_skills$skill) %>% 
    dplyr::mutate(Job_Type = as.factor(Job_Type))
  nunique <- df_cluster %>% unique() %>% nrow()
  
  y <- "Job_Type"
  set.seed(1)
  trainIndex <- caret::createDataPartition(df_cluster[[y]], p=0.7)$Resample1
  train <- df_cluster[trainIndex, ]
  test <- df_cluster[-trainIndex, ]
  
  f <- as.formula(paste(y,'~.'))
  model <- e1071::naiveBayes(f, data=train)
  
  trainPred <- predict(model, newdata = train, type = "class")
  trainTable <- table(train[[y]], trainPred)
  testPred <- predict(model, newdata=test, type="class")
  testTable <- table(test[[y]], testPred)
  trainAcc <- sum(diag(trainTable))/sum(trainTable)
  testAcc <- sum(diag(testTable))/sum(testTable)
  
  # print(n, trainAcc, testAcc)
  data.frame(n, trainAcc, testAcc)
}
training_results <- pbapply::pblapply(seq(1,50,by=1), model_training, cl = 4) %>% 
  dplyr::bind_rows()
training_results %>%
  tidyr::gather(key, value, -n) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=n, y=value, color=key))+
  ggplot2::scale_y_continuous(limits=c(0,1))
training_results[which.max(training_results$trainAcc)] #70





# Clustering Technologies ------------------------------------------------

df_cluster <-  df %>%  dplyr::select(technologies)
cl <- stats::kmeans(df_cluster[,-1], centers = 3, nstart = 20)
table(cl$cluster, df_cluster$Job_Type)
ks <- 1:20
tot_within_ss <- sapply(ks, function(k) {
  cl <- stats::kmeans(kmeans_df, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")


pr <- prcomp(x = df_cluster[,-1], scale = F)
summary(pr)
biplot(pr)
PCbiplot(pr)

binary_cor <- function(m){
  (crossprod(m) + crossprod(!m))/ nrow(m)
}
one_cor <- function(m){
  res <- data.frame()
  for(i in seq(ncol(m))){
    z <- m[,i]
    z <- apply(m,2,function(x){sum(x==z & x==1)/sum(x==1)})
    res <- rbind(res,z)
  }
  colnames(res) <- colnames(m)
  rownames(res) <- colnames(m)
  res <- as.matrix(res)
  res
}
# corr_pearson <- cor(df_cluster, method = "pearson")
# corr_bin <- binary_cor(df_cluster)
corr_one <- one_cor(df_cluster)
# corrplot::corrplot(corrmatrix, method = "square", order = "FPC")

