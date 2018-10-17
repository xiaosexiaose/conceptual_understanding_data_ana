# 载入包
library(bibliometrix)
library(ggplot2)
library(tidyr)
library(reshape2)

#数据载入及读取
My_paper_data <-
  readFiles("D:\\Data_analisis\\concept_understanding\\total.txt")
My_paper_data_biblio <-
  convert2df(My_paper_data, dbsource = "isi", format = "plaintext")
#初步分析
My_paper_first_ana <- biblioAnalysis(My_paper_data_biblio, sep = ";")
summary(My_paper_first_ana)
# 作者排名
My_paper_dominance <- dominance(My_paper_first_ana, k = 10)
# H指数计算
Authors <- My_paper_data_biblio$AU
All_authors_h_index <-
  Hindex(My_paper_data_biblio,
         Authors,
         sep = ";",
         years = 10)
# 每年出现的最多的关键词或术语
My_paper_keywordGrowth <-
  KeywordGrowth(
    My_paper_data_biblio,
    Tag = "DE",
    sep = ";",
    top = 10,
    cdf = TRUE
  )
# View(My_paper_keywordGrowth)
My_paper_keywordGrowth_plotdata <-
  gather(data = My_paper_keywordGrowth,
         key = keyword,
         value = value,
         -Year)
ggplot(My_paper_keywordGrowth_plotdata,
       aes(Year, value, group = keyword, color = keyword)) + geom_line()
# 洛特卡生产率系数
lotka_of_my_paper <- lotka(My_paper_first_ana)
lotka_of_my_paper
# 图像输出
plot(My_paper_first_ana)
# 创建书目网络
#文献耦合分析
My_paper_coupling_reference <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "coupling",
    network = "references",
    sep = ";"
  )
networkPlot(
  My_paper_coupling_reference,
  n = 20,
  Title = "文献耦合",
  type = "auto",
  label = TRUE,
  labelsize = 1,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_coupling_reference,type = "association")
# networkStat(My_paper_coupling_reference)
# 来源耦合
My_paper_coupling_source <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "coupling",
    network = "sources",
    sep = ";"
  )
networkPlot(
  My_paper_coupling_source,
  n = 20,
  Title = "来源耦合",
  type = "auto",
  label = TRUE,
  labelsize = 1,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_coupling_source,type = "association")
# networkStat(My_paper_coupling_source)
#作者合作关系
My_paper_collaboration_Author <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "collaboration",
    network = "authors",
    sep = ";"
  )
networkPlot(
  My_paper_collaboration_Author,
  n = 10,
  Title = "作者合作关系",
  type = "auto",
  label = TRUE,
  labelsize = 0.5,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_collaboration_Author,type = "association")
# networkStat(My_paper_collaboration_Author)
# 文献共引关系
My_paper_co_citation_reference <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "co-citation",
    network = "references",
    sep = ";"
  )
networkPlot(
  My_paper_co_citation_reference,
  n = 10,
  Title = "文献共引关系",
  type = "auto",
  label = TRUE,
  labelsize = 0.5,
  label.color = TRUE,
  size.cex = TRUE
)
# 关键词共现关系
My_paper_co_occurence_keyword <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "co-occurrences",
    network = "keywords",
    sep = ";"
  )
networkPlot(
  My_paper_co_occurence_keyword,
  n = 25,
  Title = "文献共引关系",
  type = "auto",
  label = TRUE,
  labelsize = 0.8,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_co_occurence_keyword,type = "association")
# networkStat(My_paper_co_occurence_keyword)
# 作者关键词共现关系
My_paper_co_occurence_author_keyword <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "co-occurrences",
    network = "author_keywords",
    sep = ";"
  )
networkPlot(
  My_paper_co_occurence_author_keyword,
  n = 25,
  Title = "作者关键词共现关系",
  type = "auto",
  label = TRUE,
  labelsize = 0.8,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_co_occurence_author_keyword,type = "association")
# networkStat(My_paper_co_occurence_author_keyword)
# 摘要共现关系
My_paper_data_biblio <-
  termExtraction(
    My_paper_data_biblio,
    Field = "AB",
    remove.numbers = TRUE,
    stemming = TRUE
  )
My_paper_co_occurence_abstract <-
  biblioNetwork(My_paper_data_biblio,
                analysis = "co-occurrences",
                network = "abstracts")
networkPlot(
  My_paper_co_occurence_abstract,
  n = 25,
  Title = "摘要共现关系",
  type = "auto",
  label = TRUE,
  labelsize = 0.8,
  label.color = TRUE,
  size.cex = TRUE
)
# normalizeSimilarity(My_paper_co_occurence_abstract,type = "association")

#引用频率分布
My_paper_requency_of_paper <-
  citations(My_paper_data_biblio, field = "article", sep = ";")
#绘制科学领域概念结构
My_paper_conceptual_construct <-
  conceptualStructure(
    My_paper_data_biblio,
    field = "ID",
    method = "MCA",
    minDegree = 2,
    k.max = 5
  )
#历史共引网络
# 历史引用信息
histNetwork_of_my_paper <-
  histNetwork(My_paper_data_biblio,
              min.citations = 10,
              sep = ";")
# 输出历史引用信息
histPlot(
  histNetwork_of_my_paper,
  n = 20,
  size.cex = TRUE,
  color = TRUE,
  labelsize = 3
)
#主题演进图
My_paper_list <-
  timeslice(My_paper_data_biblio,
            breaks = c(2000, 2012))
View(My_paper_list)
#一组
biblimatrix <-
  biblioNetwork(My_paper_list[[2]],
                analysis = "co-occurrences",
                network = "author_keywords",
                sep = ";")
S <- normalizeSimilarity(biblimatrix, type = "association")
net <-
  networkPlot(
    S,
    n = 30,
    type = "fruchterman",
    labelsize = 1,
    cluster = "walktrap",
    remove.isolates = FALSE,
    remove.multiple = FALSE,
    weighted = TRUE
  )
res1 <- thematicMap(net, biblimatrix, S)
plot(res1$map)

#二组
biblimatrix2 <-
  biblioNetwork(My_paper_list[[3]],
                analysis = "co-occurrences",
                network = "author_keywords",
                sep = ";")
S2 <- normalizeSimilarity(biblimatrix2, type = "association")
net2 <-
  networkPlot(
    S2,
    n = 30,
    type = "fruchterman",
    labelsize = 1,
    cluster = "walktrap",
    remove.isolates = FALSE,
    remove.multiple = FALSE,
    weighted = TRUE
  )
res2 <- thematicMap(net2, biblimatrix2, S2)
plot(res2$map)



#多维尺度分析
My_paper_netmatrix <-
  biblioNetwork(
    My_paper_data_biblio,
    analysis = "co-occurrences",
    network = "keywords",
    sep = ";"
  )
S <- normalizeSimilarity(My_paper_netmatrix, type = "association")
net <-
  networkPlot(
    S,
    n = 50,
    Title = "co-occurrence network",
    type = "fruchterman",
    labelsize = 0.7,
    halo = FALSE,
    cluster = "walktrap",
    remove.isolates = FALSE,
    remove.multiple = FALSE,
    noloops = TRUE,
    weighted = TRUE
  )
res <- thematicMap(net, My_paper_netmatrix, S)
plot(res$map)

nexus<-thematicEvolution(res1,res2)
