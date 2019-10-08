
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


# Clustering Attempt ------------------------------------------------------

df_cluster <- df %>%
  dplyr::select(Job_Type, technologies) %>% 
  dplyr::mutate(Job_Type = as.numeric(as.factor(Job_Type))) %>% as.matrix()
df_cluster <-  df %>%  dplyr::select(technologies)
cl <- stats::kmeans(df_cluster, centers = 3, nstart = 10)
table(cl$cluster, df$Job_Type)
ks <- 1:20
tot_within_ss <- sapply(ks, function(k) {
  cl <- stats::kmeans(kmeans_df, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")


pr <- prcomp(x = df %>% dplyr::select(technologies), scale = F)
summary(pr)
biplot(pr)
PCbiplot(pr)


# Skills ------------------------------------------------------------------

extract_skills <- function(t){
  t %>% stringr::str_replace_all("\\[|\\]|'", "") %>% stringr::str_split(',') %>% unlist %>% stringr::str_trim() %>% list()
}
skills_freq <- df %>% 
  dplyr::select(X, Job_Type, Skill) %>% 
  # dplyr::filter(Skill != "") %>% 
  dplyr::slice(1:1000) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(skill = extract_skills(Skill),
                nskills = length(skill)) %>% 
  dplyr::ungroup() %>% 
  tidyr::unnest(skill) %>% 
  dplyr::group_by(skill) %>% 
  dplyr::summarise(freq = n(),
                   mean_per_job = mean(nskills)) %>% 
  dplyr::ungroup()
wordcloud::wordcloud(words = skills_freq$skills, freq = skills_freq$freq, max.words = 200,
                     random.order = F, random.color = F, scale = c(2,0.8), rot.per = 0,
                     colors = RColorBrewer::brewer.pal(9,"Reds")[3:9], min.freq = 10)

skills_freq %>% 
  ggplot2::ggplot()+
  ggrepel::geom_text_repel(ggplot2::aes(x=mean_per_job, y=freq, label=skill,
                                        size=freq, color=mean_per_job), segment.alpha = 0)+
  ggplot2::scale_color_gradient(low="green3", high="violetred", trans = "log10",
                       guide = ggplot2::guide_colourbar(direction = "horizontal", title.position ="top")) +
  ggplot2::scale_size_continuous(range = c(3, 10), guide = FALSE) +
  # ggplot2::scale_x_log10() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position=c(.99, .99),
        legend.justification = c("right","top"),
        panel.grid.major = ggplot2::element_line(colour = "whitesmoke"))
hey <- df %>% 
  dplyr::select(X, Job_Type, Skill) %>% 
  # dplyr::slice(1:1000) %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(skills = extract_skills(Skill)) %>% 
  tidyr::unnest(skills) %>% mutate(value=1) %>% tidyr::spread(skills, value, fill=0)

n <- nrow(hey)
skills_names <- colnames(hey)[4:ncol(hey)]
hey %>% 
  dplyr::select(skills_names) %>% 
  dplyr::summarise_all(sum) %>% 
  tidyr::gather(key,value) %>% 
  dplyr::mutate(value = value/n) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(key, value),y=value), width=1)+ 
  ggplot2::coord_flip()+
  ggplot2::theme(axis.text.y = ggplot2::element_text(size=6))

hey %>% 
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
  ggplot2::facet_wrap(group~., ncol = 5, scales="free_y")+
  ggplot2::theme(axis.text.y = ggplot2::element_text(size=8),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),)
