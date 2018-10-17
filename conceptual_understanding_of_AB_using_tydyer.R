#软件包载入
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)

# 数据载入
my_origin_data <- read_excel("D:/Data_analisis/concept_understanding/data-2018-10-06.xlsx")
my_origin_data_AB<-my_origin_data[,"AB"]

# 数据清洗
number_NA<-which(is.na(my_origin_data_AB))
my_origin_data_AB <- my_origin_data_AB[-number_NA,]
number_of_article<-c(1:1679)
my_origin_data_AB<-data.frame(number_of_article,my_origin_data_AB)

# 数据分析

text_df_of_AB<- my_origin_data_AB %>%
  unnest_tokens(word,AB) %>%
  anti_join(stop_words)

text_df_of_AB %>%
  count(word,sort = TRUE) %>%
  filter(n>200 & n<=1200) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word,y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


#重要单词计算

AB_important_words<-text_df_of_AB %>%
  count(number_of_article,word) %>%
  ungroup() %>%
  bind_tf_idf(term = word,document = number_of_article,n) %>%
  arrange(desc(tf_idf))

# gongxian

text_gongxian<-my_origin_data_AB %>%
  unnest_tokens(word,AB,token = "ngrams",n=2) %>%
  separate(word,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word,word1,word2,sep=" ") %>%
  count(number_of_article,word) %>%
  filter(n>5) %>%
  separate(word,c("word1","word2"),sep = " ")


