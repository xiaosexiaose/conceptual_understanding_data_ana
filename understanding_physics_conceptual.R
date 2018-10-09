# 载入包
library(bibliometrix)
library(ggplot2)
library(magrittr)
library(tidyr)
library(readxl)
library(dplyr)
library(wordcloud2)
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
  #筛选数据函数
My_bibli_datalist<-list()
year_vector<-seq(1991,2018,1)
for (year in year_vector) {
  My_bibli_datalist[[which(year_vector==year)]]<-My_paper_data_biblio %>%
    filter(PY==year)
}
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
DE_occurences<-biblioNetwork(My_paper_data_biblio,analysis = "co-occurrences",network = "keywords",sep = ";")



AB_TM_Data<-termExtraction(My_paper_data_biblio,Field = "AB")
AB_occurences<-biblioNetwork(AB_TM_Data,analysis = "co-occurrences",network = "abstracts",sep = ";")
