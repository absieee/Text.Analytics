## libraries
library(tm)
library(RCurl)
library(XML)
library(SnowballC)
library(ggplot2)
library(NLP)
library(wordcloud)
library(Rgraphviz)
library(fpc)
library(cluster)

# set working directory and data path
setwd("C:/Users/ABS/Google Drive/Soton/sem-2/COMP6237 - Data Mining/cw2")
path_main <- getwd()
path_data <- 'C:/Users/ABS/Google Drive/Soton/sem-2/COMP6237 - Data Mining/cw2/gap-html'

# get html files as vector
files <- list.files(path_data, full.names=TRUE)
files <- files[-1]
html <- list.files(files, full.names = TRUE, pattern=".html$")
## convert html files to text
text <- htmlToText(html)

## word_corpus creation
word_corpus <- Corpus(VectorSource(text))
## word_corpus cleaning
word_corpus <- tm_map(word_corpus, stripWhitespace)
word_corpus <- tm_map(word_corpus, content_transformer(tolower))
word_corpus <- tm_map(word_corpus, removeWords, stopwords("english"))
word_corpus <- tm_map(word_corpus, stemDocument)
word_corpus <- tm_map(word_corpus, removeNumbers)
word_corpus <- tm_map(word_corpus, removePunctuation)

word_corpus2 <- tm_map(word_corpus, PlainTextDocument)

## create document term matrix
dtm <- DocumentTermMatrix(word_corpus)

## remove sparse terms from dtm
# TODO: make dtm more sparse
dtm_sparse <- removeSparseTerms(dtm, sparse=0.95)
dtm_sparse2 <- removeSparseTerms(dtm, 0.5)

## create dtm of specific word lengths in word_corpus found in specific number of docs
# TODO: change bounds and word lengths to get better dtm
dtmr <-DocumentTermMatrix(word_corpus, control=list(wordLengths=c(4, 20), bounds = list(global = c(100,600))))
dtmr_sparse <- removeSparseTerms(dtmr, sparse=0.1)

## 200 most frequent terms for original dtm
freqTerms200 = findFreqTerms(dtm, lowfreq = 200)
write.csv(freqTerms200, file = "freqTerms200.csv")

## get most frequent terms for sparse dtm 
freq <- sort(colSums(as.matrix(dtm_sparse)), decreasing=TRUE)
write.csv(freq, file = "frequencies.csv")
head(freq, 50)
tail(freq, 50)
head(table(freq), 50)
tail(table(freq), 50)

## word frequencies as data frame object
wf <- data.frame(word=names(freq), freq=freq)

## plot word frequencies graph
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))

## associated terms and correlation graph
assocsRome = findAssocs(dtm_sparse,term="rome",0.1)
plot(dtm_sparse, terms = names(findAssocs(dtm_sparse,term="rome",0.1)[["rome"]]), corThreshold = 0.80, attrs=list(node=list(label="foo", fillcolor="lightgreen", fontsize="16", shape="ellipse"), edge=list(color="black"), graph=list(rankdir="LR")))
plot(dtm_sparse, corThreshold = 0.80)

## wordcloud creation
set.seed(666)
# plot words with freq more than min.freq
wordcloud(names(freq), freq, min.freq = 1000, max.words = Inf, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(7,.4), rot.per=0.2)
# plot words occurring at least n times
wordcloud(names(freq), freq, min.freq = 10000, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(7,.4), rot.per=0.2)
# plot n most occuring words
wordcloud(names(freq), freq, max.words = 100, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(7,.4), rot.per=0.2)

## create term document matrix
tdm <- TermDocumentMatrix(word_corpus)
tdms <- removeSparseTerms(tdm, sparse=0.75)

## clustering - simple method
d <- dist(as.matrix(tdms), method = "euclidian")
set.seed(123)
hc <- hclust(d, "ward.D2")
plot(hc)

groups <- cutree(hc, k=2)
rect.hclust(hc, k=2, border = 'blue')

## hierarchical clustering - long method 
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.

# calculate distance between words
dist_words <- dist(t(dtm_sparse), method="euclidian")
fit <- hclust(d=d, method="ward.D")   
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

## k-means clustering   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)

dist_kmeans <- dist(t(dtm_sparse), method="euclidian")   
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
