# 载入包
library(bibliometrix)
library(ggplot2)
library(magrittr)
library(tidyr)
library(readxl)
library(dplyr)
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