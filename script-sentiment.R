library("tm")
library("Snowball")
library("SnowballC")
library("RWeka")
library("rJava")
library("RWekajars")
library("sentiment")
library(plyr)
library("ggplot2")
library(wordcloud)
library(RColorBrewer)

texto <- readLines("tec-feed.txt")
# convert texto to a corpus

df <- do.call("rbind", lapply(texto, as.data.frame))

myCorpus <- Corpus(VectorSource(df$text))
#myCorpus <- VCorpus(DirSource("comentarios", encoding = "UTF-8"), readerControl = list(language = "eng"))



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
myCorpusTest <- myCorpus
#dput(myCorpusTest, file="myCorpusTest.txt")
#dput(myCorpusCopy, file="myCorpusCopy.txt")

#myCorpus <- tm_map(myCorpus, stemDocument)
#dput(myCorpus, file="myCorpus.txt")
# inspect results !!
#inspect(myCorpus)

# stem completion
#a <- c("mining", "miners", "mining")
#b <- stemDocument(a, language="eng")
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)
#myCorpus <- tm_map(myCorpus, stemCompletion(myCorpus, dictionary=myCorpusCopy))

#myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
#dtm <- DocumentTermMatrix(myCorpus)




# myTdm
# inspect frequent words
# findFreqTerms(myTdm, lowfreq=10)



#termFrequency <- rowSums(as.matrix(myTdm))
#termFrequency <- subset(termFrequency, termFrequency>=10)
#library(ggplot2)
#df <- data.frame(term=names(termFrequency), freq=termFrequency)
#ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
#xlab("Terms") + ylab("Count") + coord_flip()



# which words are associated with "yes"?
#findAssocs(myTdm, yes, 0.25)

#library(wordcloud)
#m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
#wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
#set.seed(375) # to make it reproducible
#grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
#wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
#colors=grayLevels)



#Dendogram

# remove sparse terms
#myTdm2 <- removeSparseTerms(myTdm, sparse=0.65)
#m2 <- as.matrix(myTdm2)
# cluster terms
#distMatrix <- dist(scale(m2))
#fit <- hclust(distMatrix, method="ward")

#plot(fit)
# cut tree into 10 clusters
#rect.hclust(fit, k=10)
#(groups <- cutree(fit, k=10))

# Clustering with the k-means Algorithm
# transpose the matrix to cluster documents k-means
#m3 <- t(m2)
# set a fixed random seed
#set.seed(122)
# k-means clustering of docs
#k <- 10
#kmeansResult <- kmeans(m3, k)
# cluster centers
#round(kmeansResult$centers, digits=3)

#for (i in 1:k) {
#cat(paste("cluster ", i, ": ", sep=""))
#s <- sort(kmeansResult$centers[i,], decreasing=T)
#cat(names(s)[1:3], "\n")
# print the tweets of every cluster
# print(rdmTweets[which(kmeansResult$cluster==i)])
#}

#Clustering  with the k-medoids Algorithm
#library(fpc)
# partitioning around medoids with estimation of number of clusters
#pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
#(k <- pamResult$nc)
#pamResult <- pamResult$pamobject
# print cluster medoids
#for (i in 1:k) {
#cat(paste("cluster", i, ": "))
#cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
# print tweets in cluster i
# print(rdmTweets[pamResult$clustering==i])
#}


#************* SENTIMENT***************



texto<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)

# remove unnecessary spaces
#texto = gsub("[ \t]{2,}", "", texto)
#texto = gsub("^\\s+|\\s+$", "", texto)

# classify emotion
class_emo = classify_emotion(texto, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(texto, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


# data frame with results
sent_df = data.frame(text=texto, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of emotions
#ggplot(sent_df, aes(x=emotion)) 
#geom_bar(aes(y=..count.., fill=emotion)) 
#scale_fill_brewer(palette="Dark2") 

#ggtitle("Sentiment Analysis of Tweets on Twitter about BJP") +
#theme(legend.position="right") + ylab("Number of Tweets") + xlab(‘Emotion Categories’)
#labs(x="emotion categories", y="number of tweets") 
#title = "Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)" 
#     theme(plot.title = element_text(size=12))
qplot(factor(sent_df$text), sent_df$emotion, geom="bar", stat="identity")


