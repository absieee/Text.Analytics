# libraries
library(RCurl)  # library for html to text function
library(XML)    # library for html to text function
library(tm)     # library for corpus
library(SnowballC)  # library for 
library(ggplot2)    # library for
library(NLP)        # library for
library(wordcloud)  # library for
library(Rgraphviz)  # library for
library(fpc)        # library for
library(cluster)    # library for
library(sos)        # library for
library(qlcMatrix)  # library for
library(lsa)        # library for

# set paths
setwd(path_main)
path_main <- 'C:/Users/ABS/Google Drive/Soton/sem-2/COMP6237 - Data Mining/cw2'
path_html <- 'C:/Users/ABS/Google Drive/Soton/sem-2/COMP6237 - Data Mining/cw2/gap-html'
path_text <- 'C:/Users/ABS/Google Drive/Soton/sem-2/COMP6237 - Data Mining/cw2/text'

#-------------HTML TO TEXT CONVERSION-------------#
# get paths to html files
files <- list.files(path_data, full.names=TRUE)
files <- files[-1]

# create vector of paths for each doc
for (n in 1:(length(files)-1)){
  html <- paste("html", n, sep = "")
  assign(html, list.files(files[n], full.names = TRUE, pattern=".html$"))
}

# html paths as list
htmlfiles <- list(html1,html2,html3,html4,html5,html6,html7,html8,html9,html10,html11,html12,html13,html14,html15,html16,html17,html18,html19,html20,html21,html22,html23,html24)

## convert html files to text and save
setwd(path_text)
# html to text conversion
doc1 <- htmlToText(html1)
write(doc1, file = "doc1.txt")
doc2 <- htmlToText(html2)
write(doc2, file = "doc2.txt")
doc3 <- htmlToText(html3)
write(doc3, file = "doc3.txt")
doc4 <- htmlToText(html4)
write(doc4, file = "doc4.txt")
doc5 <- htmlToText(html5)
write(doc5, file = "doc5.txt")
doc6 <- htmlToText(html6)
write(doc6, file = "doc6.txt")
doc7 <- htmlToText(html7)
write(doc7, file = "doc7.txt")
doc8 <- htmlToText(html8)
write(doc8, file = "doc8.txt")
doc9 <- htmlToText(html9)
write(doc9, file = "doc9.txt")
doc10 <- htmlToText(html10)
write(doc10, file = "doc10.txt")
doc11 <- htmlToText(html11)
write(doc11, file = "doc11.txt")
doc12 <- htmlToText(html12)
write(doc12, file = "doc12.txt")
doc13 <- htmlToText(html13)
write(doc13, file = "doc13.txt")
doc14 <- htmlToText(html14)
write(doc14, file = "doc14.txt")
doc15 <- htmlToText(html15)
write(doc15, file = "doc15.txt")
doc16 <- htmlToText(html16)
write(doc16, file = "doc16.txt")
doc17 <- htmlToText(html17)
write(doc17, file = "doc17.txt")
doc18 <- htmlToText(html18)
write(doc18, file = "doc18.txt")
doc19 <- htmlToText(html19)
write(doc19, file = "doc19.txt")
doc20 <- htmlToText(html20)
write(doc20, file = "doc20.txt")
doc21 <- htmlToText(html21)
write(doc21, file = "doc21.txt")
doc22 <- htmlToText(html22)
write(doc22, file = "doc22.txt")
doc23 <- htmlToText(html23)
write(doc23, file = "doc23.txt")
doc24 <- htmlToText(html24)
write(doc24, file = "doc24.txt")

#--------CORPUS CREATION AND CLEANING--------#
# create corpus
corpus <- Corpus(DirSource(path_text))
# corpus cleaning
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "ocr", "output"))

#-------------DTM-------------#
# create document term matrix
dtm <- DocumentTermMatrix(corpus)
dtms95 <- removeSparseTerms(dtm, 0.95) # remove sparse terms
dtms1 <- removeSparseTerms(dtm, 0.1) # 1755 terms
dtms15 <- removeSparseTerms(dtm, 0.15) #2141 terms
dtms5 <- removeSparseTerms(dtm, 0.5) #5090 terms
# tf-idf + word limit weighted dtm
dtm_wl_tfidf <- DocumentTermMatrix(corpus, control = list(wordLengths=c(3, Inf), weighting = function(x) weightTfIdf(x, normalize = TRUE)))
dtm_wl_tfidf_sparse5 <- removeSparseTerms(dtm_wl_tfidf, 0.5)
dtm_wl_tfidf_sparse1 <- removeSparseTerms(dtm_wl_tfidf, 0.1)
dtm_wl_tfidf_sparse15 <- removeSparseTerms(dtm_wl_tfidf, 0.15)
#-------------WORD FREQUENCY PLOTS-------------#
# get most frequent terms for sparse dtm
freq <- sort(colSums(as.matrix(dtm_tfidf_sparse)), decreasing=TRUE)
write.csv(freq, file = "frequencies.csv")
head(freq, 50)
tail(freq, 50)
head(table(freq), 50)
tail(table(freq), 50)

# word frequencies as data frame object
wf <- data.frame(word=names(freq), freq=freq)
# plot word frequencies graph
p <- ggplot(subset(wf, freq>20), aes(word, freq))    
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#-------------FINDING ASSOCS-------------#
# finding associated terms
assocs = findAssocs(dtmrs,c("king", "roman"),0.90)
# plot correlation graph
plot(dtmrs, corThreshold = 0.80)
plot(dtmrs, terms = names(findAssocs(dtmrs,c("king", "general"),0.90)[["general"]]), corThreshold = 0.80, attrs=list(node=list(label="foo", fillcolor="lightblue", fontsize="16", shape="rectangle"), edge=list(color="black"), graph=list(rankdir="LR")))

#-------------WORDCLOUD-------------#
## wordcloud
set.seed(666)
# plot words with freq more than min.freq
wordcloud(names(freq), freq, min.freq = 2000, max.words = Inf, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(7,.4), rot.per=0.2)
# plot n most occuring words
wordcloud(names(freq), freq, max.words = 10, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(7,.4), rot.per=0.2)

#-------------DISTANCE CALCS-------------#
# manhattan dist
dist_man <- dist(as.matrix(dtms), method = "manhattan")
# euc dist
dist_euc <- dist(as.matrix(dtms), method = "euclidian")

# cosine distance
d_cos1 <- cosine(as.matrix(t(dtms1)))
dist_cos1 <- as.dist(1-d_cos1)
# cosine distance
d_cos15 <- cosine(as.matrix(t(dtms15)))
dist_cos15 <- as.dist(1-d_cos15)
# cosine distance
d_cos5 <- cosine(as.matrix(t(dtms5)))
dist_cos5 <- as.dist(1-d_cos5)

# word limit + weighted cosine distance
d_cos_wl_tfidf1 <- cosine(as.matrix(t(dtm_wl_tfidf_sparse1)))
dist_cos_wl_tfidf1 <- as.dist(1-d_cos_wl_tfidf1)
# different sparsity
d_cos_wl_tfidf5 <- cosine(as.matrix(t(dtm_wl_tfidf_sparse5)))
dist_cos_wl_tfidf5 <- as.dist(1-d_cos_wl_tfidf5)
# different sparsity
d_cos_wl_tfidf15 <- cosine(as.matrix(t(dtm_wl_tfidf_sparse15)))
dist_cos_wl_tfidf15 <- as.dist(1-d_cos_wl_tfidf15)

#--------------MDS-----------------#
set.seed(999)
# manhattan dist
fit <- cmdscale(dist_man, eig=TRUE, k=2)
# euc dist
fit <- cmdscale(dist_euc, eig=TRUE, k=2)

# cos dist
fit <- cmdscale(dist_cos1, eig=TRUE, k=2) # k is the number of dim
fit <- cmdscale(dist_cos15, eig=TRUE, k=2) # k is the number of dim
fit <- cmdscale(dist_cos5, eig=TRUE, k=2) # k is the number of dim

# wl+tf_idf
fit <- cmdscale(dist_cos_wl_tfidf5, eig=TRUE, k=2) # k is the number of dim
fit <- cmdscale(dist_cos_wl_tfidf15, eig=TRUE, k=2) # k is the number of dim
fit <- cmdscale(dist_cos_wl_tfidf1, eig=TRUE, k=2) # k is the number of dim

fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="x", ylab="y", main="Metric MDS",	type="n")
text(x, y, labels = row.names(dtm), cex=0.9)


#-----------------H-CLUSTERING-----------------#
set.seed(123)

# man cluster
hc_man <- hclust(dist_man, "ward.D2")
plot(hc_man)

# euc cluster
hc_euc <- hclust(dist_euc, "ward.D2")
plot(hc_euc)

# same clusters for dtms and cos_wl
# cluster for dtms1
hc_cos1 <- hclust(dist_cos1, "ward.D2")
plot(hc_cos1)
# plot cluster borders
groups <- cutree(hc_cos1, k=7)
rect.hclust(hc_cos1, k=7, border = 'green')

# cluster for dtms15
hc_cos15 <- hclust(dist_cos15, "ward.D2")
plot(hc_cos15)
# plot cluster borders
groups <- cutree(hc_cos15, k=9)
rect.hclust(hc_cos15, k=9, border = 'yellow')

# cluster for dtms5
hc_cos5 <- hclust(dist_cos5, "ward.D2")
plot(hc_cos5)
# plot cluster borders
groups <- cutree(hc_cos5, k=6)
rect.hclust(hc_cos5, k=6, border = 'red')


# same results obtained for tfidf and wl+tfidf dtm
# tfidf + wl cluster
hc_cos_wl_tfidf1 <- hclust(dist_cos_wl_tfidf1, "ward.D2")
plot(hc_cos_wl_tfidf1)
# plot cluster borders
groups <- cutree(hc_cos_wl_tfidf1, k=5)
rect.hclust(hc_cos_wl_tfidf1, k=5, border = 'cyan')

# tfidf + wl cluster - sparsity change
hc_cos_wl_tfidf15 <- hclust(dist_cos_wl_tfidf15, "ward.D2")
plot(hc_cos_wl_tfidf15)
# plot cluster borders
groups <- cutree(hc_cos_wl_tfidf15, k=4)
rect.hclust(hc_cos_wl_tfidf15, k=4, border = 'blue')

# tfidf + wl cluster - sparsity change
hc_cos_wl_tfidf5 <- hclust(dist_cos_wl_tfidf5, "ward.D2")
plot(hc_cos_wl_tfidf5)
# plot cluster borders
groups <- cutree(hc_cos_wl_tfidf5, k=6)
rect.hclust(hc_cos_wl_tfidf5, k=6, border = 'darkblue')

# cluster graph as phylo tree
# load package ape
library(ape)
# plot basic tree
plot(as.phylo(hc), cex = 0.9, label.offset = 0)

#--------------K-MEANS-CLUSTERING-----------------#
# plot k-means
set.seed(333)
# kmc for man dist
kfit_man <- kmeans(dist_man, 4)
plotcluster(dist_man, kfit_man$cluster)
clusplot(as.matrix(dist_man), kfit_man$cluster, color=T, shade=T, labels=2, lines=0)

# kmc for euc dist
kfit_euc <- kmeans(dist_euc, 4)
plotcluster(dist_euc, kfit_euc$cluster)
clusplot(as.matrix(dist_euc), kfit_euc$cluster, color=T, shade=T, labels=2, lines=0)

# same results obtained for cos and cos_wl dtm
# kmc for cos dist
kfit_cos1 <- kmeans(dist_cos1, 4)
plotcluster(dist_cos1, kfit_cos1$cluster)
clusplot(as.matrix(dist_cos1), kfit_cos1$cluster, color=T, shade=T, labels=2, lines=0)

# kmc for weighted + word limit dist
kfit_cos_wl_tfidf5 <- kmeans(dist_cos_wl_tfidf5, 4)
plotcluster(dist_cos_wl_tfidf5, kfit_cos_wl_tfidf5$cluster)
clusplot(as.matrix(dist_cos_wl_tfidf5), kfit_cos_wl_tfidf5$cluster, color=T, shade=T, labels=2, lines=0)
# sparse 1
kfit_cos_wl_tfidf1 <- kmeans(dist_cos_wl_tfidf1, 5)
plotcluster(dist_cos_wl_tfidf1, kfit_cos_wl_tfidf1$cluster)
clusplot(as.matrix(dist_cos_wl_tfidf1), kfit_cos_wl_tfidf1$cluster, color=T, shade=T, labels=2, lines=0)
# sparse 15
kfit_cos_wl_tfidf15 <- kmeans(dist_cos_wl_tfidf15, 5)
plotcluster(dist_cos_wl_tfidf15, kfit_cos_wl_tfidf15$cluster)
clusplot(as.matrix(dist_cos_wl_tfidf15), kfit_cos_wl_tfidf15$cluster, color=T, shade=T, labels=2, lines=0)