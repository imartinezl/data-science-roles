library(dplyr)

df_mcq_questions <- read.csv(file='multiple_choice_responses.csv',header=TRUE,nrow = 1) 
df_mcq <- read.csv(file='multiple_choice_responses.csv',skip=2,header=FALSE) 
colnames(df_mcq) <- colnames(df_mcq_questions) 

df_mcq %>% 
  dplyr::group_by(Q5) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(Q5,n),y=n))+
  ggplot2::coord_flip()+
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(hjust=1,angle=0)
  )

df_mcq %>% 
  dplyr::group_by(Q5) %>% 
  dplyr::summarise(n = n())

qs <- colnames(df_mcq_questions) %>% stringr::str_subset("Q9_P")# %>% paste(collapse=", ")
qs_text <- sapply(qs, function(q){
  df_mcq %>% dplyr::select(q) %>% unique() %>% dplyr::filter(get(q) != "") %>% dplyr::pull() %>% as.character()
})
qs_text %>% as.data.frame() %>% add_rownames()
df_mcq %>% 
  dplyr::mutate(id = row_number()) %>% 
  tidyr::gather(key, value, Q9_Part_1, Q9_Part_2, Q9_Part_3, Q9_Part_4, Q9_Part_5, Q9_Part_6, Q9_Part_7, Q9_Part_8) %>% 
  dplyr::select(id, Q5, key, value) %>% 
  dplyr::mutate(value = value != "") %>% 
  dplyr::filter(value) %>% 
  dplyr::group_by(Q5,key) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(Q5,n),y=n,fill=key),position = "fill")+
  ggplot2::coord_flip()


df_mcq %>% 
  dplyr::mutate(id = row_number()) %>% 
  tidyr::gather(key, value, Q9_Part_1, Q9_Part_2, Q9_Part_3, Q9_Part_4, Q9_Part_5, Q9_Part_6) %>% 
  dplyr::select(id, Q5, key, value) %>% 
  dplyr::mutate(value = value != "") %>% 
  dplyr::filter(value) %>% 
  dplyr::group_by(Q5,key) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>% 
  ggplot2::ggplot()+
  ggplot2::geom_polygon(ggplot2::aes(x=key, y=n, group=Q5, col=Q5), fill=NA)+
  ggplot2::geom_point(ggplot2::aes(x=key, y=n, group=Q5, col=Q5), fill=NA)+
  ggplot2::coord_polar()


df_mcq %>% 
  dplyr::mutate(id = row_number()) %>% 
  tidyr::gather(key, value, Q9_Part_1, Q9_Part_2, Q9_Part_3, Q9_Part_4, Q9_Part_5, Q9_Part_6) %>% 
  dplyr::select(id, Q5, key, value) %>% 
  dplyr::mutate(value = value != "") %>% 
  dplyr::filter(value) %>% 
  dplyr::group_by(Q5,key) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter( n == max(n)) %>%
  dplyr::ungroup() %>% 
  merge(qs_text, all.x=T, all.y=F) %>% View
