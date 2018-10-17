# 载入包
library(bibliometrix)
library(ggplot2)
library(magrittr)
library(tidyr)
library(readxl)
library(dplyr)
library(wordcloud2)
library(plyr)
# 数据载入
My_paper_data <-
  readFiles("D:\\Data_analisis\\concept_understanding\\total.txt")
My_paper_data_biblio <-
  convert2df(My_paper_data, dbsource = "isi", format = "plaintext")
# 初步描述性分析
My_paper_intial_ana<-biblioAnalysis(My_paper_data_biblio,sep = ";")
#作者关键字出现的次数及词云显示
Authors_keyword_fre<-My_paper_intial_ana$ID
Authors_keyword_fre<-as.data.frame(Authors_keyword_fre)
Authors_keyword_fre_word<-Authors_keyword_fre %>%
  filter(Freq>=15)
wordcloud2(Authors_keyword_fre_word,
           fontFamily = "times new roman",
           backgroundColor = "white",
           shape = "circle"
        )+WCtheme(1)
# 描述每年文献的数目-图表
ggplot(data = My_paper_data_biblio,aes(x=PY))+geom_bar()+scale_x_continuous(limits = c(1990,2018),breaks = seq(1990,2018,5))+xlab("Year")+ylab("Number")+scale_y_continuous(breaks = seq(0,180,10))+theme_bw()
# 每年发文量的表格信息-以及1990年之前的文章数目
Number_of_paper_every_year<-My_paper_data_biblio %>%
  group_by(PY) %>%
  count(PY) %>%
  filter(PY>1990)
Number_of_paper_every_year_before1945<-Number_of_paper_every_year %>%
  filter(PY<=1990) %>%
  colSums()
# 作者关键词共现年度演变研究
#Yearly occurrences of top keyword
topKeyword<-KeywordGrowth(My_paper_data_biblio,Tag = "DE",sep = ";",top = 10)
topKeyword_melt<-gather(topKeyword,key = "Key_Words",value = "Occurences",-Year)

ggplot(data = topKeyword_melt,
       aes(x=Year,
           y=Occurences,
           group=Key_Words,
           color=Key_Words))+geom_line(size=1)+
  scale_y_continuous(limits = c(0,30))+
  theme_bw()
# 关键词共现-DE and AB
My_paper_data_biblio_ABTM<-termExtraction(My_paper_data_biblio,Field = "AB")
DE_occurences<-biblioNetwork(My_paper_data_biblio_ABTM,analysis = "co-occurrences",
                             network = "keywords",
                             sep = ";")
DE_occurences_network<-networkPlot(DE_occurences,
                                   degree = 10,
                                   type = "fruchterman",
                                   labelsize = 0.7,
                                   cluster = "walktrap",
                                   weighted = TRUE
                                   )


#ThematicMap
S_map<-normalizeSimilarity(DE_occurences)
ThematicaMap_of_paper<-thematicMap(DE_occurences_network,DE_occurences,S_map,minfreq = 5)
plot(ThematicaMap_of_paper$map)
View(ThematicaMap_of_paper$clusters)


#计算中心性以及密度的函数
#2005~2018 两年能够有明显的聚类结果
My_paper_data_biblio_ana_dada<-My_paper_data_biblio %>%
  filter(PY>=1991)

list_of_my_paper<-timeslice(My_paper_data_biblio_ana_dada,k=14)


My_paper_data_biblio_ana_dada_1<-My_paper_data_biblio %>%
  filter(PY>=1991 & PY<=2004)


list_of_my_paper_1<-timeslice(My_paper_data_biblio_ana_dada_1,k=4)


list_of_clusters_1<-list()
groups_of_number_1<-c(8:14)
for (groups_of_paper in groups_of_number_1) {
  
  function_NetMatrix<-biblioNetwork(list_of_my_paper[[groups_of_paper]],
                                                  analysis = "co-occurrences",
                                                  network = "keywords",
                                                  sep = ";")
  function_Net<-networkPlot(function_NetMatrix,
                            degree = 10,
                            type = "fruchterman",
                            labelsize = 0.7,
                            cluster = "walktrap",
                            weighted = TRUE)
  function_normal<-normalizeSimilarity(function_NetMatrix,type = "association")
  
  function_thematic<-thematicMap(function_Net,function_NetMatrix,function_normal)
  
  number<-which(groups_of_paper==groups_of_number_1)
  
  list_of_clusters_1[[number]]<-function_thematic$clusters
}

#1991~2004 三年有明显结果

list_of_clusters_2<-list()
groups_of_number_2<-c(2:4)
for (groups_of_paper in groups_of_number_2) {
  
  function_NetMatrix<-biblioNetwork(list_of_my_paper_1[[groups_of_paper]],
                                    analysis = "co-occurrences",
                                    network = "keywords",
                                    sep = ";")
  function_Net<-networkPlot(function_NetMatrix,
                            degree = 10,
                            type = "fruchterman",
                            labelsize = 0.7,
                            cluster = "walktrap",
                            weighted = TRUE)
  function_normal<-normalizeSimilarity(function_NetMatrix,type = "association")
  
  function_thematic<-thematicMap(function_Net,function_NetMatrix,function_normal)
  
  number<-which(groups_of_paper==groups_of_number_2)
  
  list_of_clusters_2[[number]]<-function_thematic$clusters
}

#将两个list上的函数分别进行转换

for (n in 1:3) {
  list_of_clusters_2[[n]]$group<-n
  View(list_of_clusters_2[[n]])
}
for (n in 1:7) {
  list_of_clusters_1[[n]]$group<-(n+3)
  View(list_of_clusters_1[[n]])
}

cluster1_data_frame_1<-rbind(list_of_clusters_2[[1]],list_of_clusters_2[[2]])
cluster1_data_frame<-rbind(cluster1_data_frame_1,list_of_clusters_2[[3]])

cluster2_data_frame<-list_of_clusters_1[[1]]
for (n in 2:7) {
  cluster2_data_frame<-rbind(cluster2_data_frame,list_of_clusters_1[[n]])
}

trend_of_paper<-rbind(cluster1_data_frame,cluster2_data_frame)
trend_of_paper$group<-factor(trend_of_paper$group)

write.csv(trend_of_paper,file = "D:\\Data_analisis\\concept_understanding\\MCA.csv")


trend_of_paper_group_by_group<-trend_of_paper %>%
  arrange(group)
View(trend_of_paper_group_by_name)


MCA_data <- read_excel("D:/Data_analisis/concept_understanding/MCA.xlsx")
MCA_data$group<-factor(MCA_data$group)

MCA_data<-MCA_data %>%
  arrange(group)



ggplot(data = MCA_data,
       mapping = aes(x=MCA_data$centrality,
                     y=MCA_data$density,
                     group=factor(MCA_data$name),
                     color=MCA_data$group))+
  geom_path(size=1)+geom_point(size=MCA_data$freq)+
  geom_text(label=paste(MCA_data$name),colour="black",size=2)+
  theme_bw()+
  geom_hline(yintercept = mean(MCA_data$density),linetype=2,size=1)+
  geom_vline(xintercept = mean(MCA_data$centrality),linetype=2,size=1)


ggplot(data = trend_of_paper,
       mapping = aes(x=trend_of_paper$centrality,
                     y=trend_of_paper$density,
                     group=factor(trend_of_paper$name),
                     color=trend_of_paper$group))+
  geom_path(size=1)+geom_point(size=trend_of_paper$freq)+
  geom_text(label=paste(trend_of_paper$name),colour="black",size=2)+
  theme_bw()+
  geom_hline(yintercept = mean(MCA_data$density),linetype=2,size=1)+
  geom_vline(xintercept = mean(MCA_data$centrality),linetype=2,size=1)


# foundations

histResult<-histNetwork(My_paper_data_biblio,min.citations = 10,sep = ";")




histPlot(histResult,n=30,size.cex = TRUE,color = TRUE,labelsize = 3)

citation_of_paper<-biblioNetwork(My_paper_data_biblio,
                                 analysis = "co-citation",
                                 network = "references",
                                 sep = ";")

networkPlot(citation_of_paper,
            n =  25,
            type = "fruchterman",
            weighted = TRUE,
            cluster = "walktrap",
            remove.isolates = TRUE
            )

networkPlot(citation_of_paper,
            n =  30,
            type = "vosviewer",
            weighted = TRUE,
            cluster = "walktrap",
            remove.isolates = TRUE,
            vos.path = "D:\\Data_analisis\\concept_understanding\\VOSviewer_1.6.9_jar")


