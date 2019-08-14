library("tm")
library("Snowball")
library("SnowballC")
library("RWeka")
library("rJava")
library("RWekajars")


myCorpus <- VCorpus(DirSource("comentarios", encoding = "UTF-8"), readerControl = list(language = "eng"))



# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))



# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)


# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)


# remove URLs
#removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
#myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("english"), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words

#myCorpus <- tm_map(myCorpus, stemDocument)

# inspect results !!
#inspect(myCorpus)

# stem completion
#a <- c("mining", "miners", "mining")
#b <- stemDocument(a, language="eng")
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)
#myCorpus <- tm_map(myCorpus, stemCompletion(myCorpus, dictionary=myCorpusCopy))

myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

# myTdm
# inspect frequent words
# findFreqTerms(myTdm, lowfreq=10)



termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "yes"?
#findAssocs(myTdm, yes, 0.25)

library(wordcloud)
m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
#set.seed(375) # to make it reproducible
#grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
#wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
#colors=grayLevels)


#-----------Dendrogram--------
# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.70)
m2 <- as.matrix(myTdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")

plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=5)
(groups <- cutree(fit, k=5))

#----------------------network graph ------------------
plot(myTdm2,terms=findFreqTerms(myTdm2, lowfreq=4)[1:9], corThreshold=0.8)

#---------------------------------------------

# Clustering with the k-means Algorithm
# transpose the matrix to cluster documents k-means
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of docs
k <- 10
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=4)

for (i in 1:k) {
cat(paste("cluster ", i, ": ", sep=""))
s <- sort(kmeansResult$centers[i,], decreasing=T)
cat(names(s)[1:3], "\n")
# print the tweets of every cluster
# print(rdmTweets[which(kmeansResult$cluster==i)])
}

#Clustering  with the k-medoids Algorithm
library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
cat(paste("cluster", i, ": "))
cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
#print tweets in cluster i
#print(rdmTweets[pamResult$clustering==i])
}








