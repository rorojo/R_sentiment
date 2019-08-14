#this script allow cloud and word frec diagrams. User can define cloud by sentiment or by polarity removing # at line 102 and 108

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



#************* SENTIMENT***************

texto1 <- readLines("files/Feelings About Working Relationship With ARRIS.txt")
texto2 <- readLines("files/ARRIS Communicating Their Product Portfolio Post-Transformation.txt")
texto3 <- readLines("files/ARRIS Performance Post-Transformation.txt")
texto4 <- readLines("files/ARRIS Team Giving Right Level of Engagement Post-Transformation.txt")
texto5 <- readLines("files/Confidence in Product Development Roadmap Meeting Future Technology Requirements.txt")
texto6 <- readLines("files/Factors in Deciding Which Supplier to Use For Product Needs.txt")
texto7 <- readLines("files/How ARRIS Compares to Competitive Offerings Alternatives.txt")
texto8 <- readLines("files/How Other Suppliers Compare on These Top Business Challenges.txt")
texto9 <- readLines("files/How Well ARRIS Aligns With Top Business Challenges Is a Trusted Advisor.txt")
texto10 <- readLines("files/How Well ARRIS Current Product Development Roadmap Is Aligned w-Org Strategies.txt")
texto11 <- readLines("files/Introduction-Context Setting.txt")
texto12 <- readLines("files/Key Areas Differentiating Other Suppliers From ARRIS.txt")
texto13 <- readLines("files/Other Notes.txt")
texto14 <- readLines("files/Other Opportunities For ARRIS to Add Value-Be More Relevant to Organization.txt")
texto15 <- readLines("files/Relationship With ARRIS Over Last 12 Months.txt")
texto16 <- readLines("files/Suppliers Viewed As Emerging or Declining.txt")
texto17 <- readLines("files/Top Business Challenges Faced By Organization.txt")
texto18 <- readLines("files/Where ARRIS Fell Short of Expectations in the Past Year.txt")
texto19 <- readLines("files/Where ARRIS Has Exceeded Expectations in the Past Year.txt")
texto20 <- readLines("files/Where ARRIS is Providing Most Value.txt")
texto21 <- readLines("files/net.txt")
texto22 <- readLines("filescustomerinsight/Change ARRIS Needs To Make To Improve The Way It Does Business.txt")
texto23 <- readLines("filescustomerinsight/How to Modify Software Quality to Better Meet Needs.txt")
texto24 <- readLines("filescustomerinsight/How to Modify Hardware Quality to Better Meet Needs.txt")
texto25 <- readLines("filescustomerinsight/How to Modify Sales Pre-Sales Support Process to Better Meet Needs.txt")
texto26 <- readLines("filescustomerinsight/How to Modify Marketing Support to Better Meet Needs.txt")
texto27 <- readLines("filescustomerinsight/How to Improve Ordering Delivery Process to Better Meet Needs.txt")
texto28 <- readLines("filescustomerinsight/How to Modify Support Services to Better Meet Needs.txt")
texto29 <- readLines("filescustomerinsight/How to Modify Technical Documentation to Better Meet Needs.txt")
texto30 <- readLines("filescustomerinsight/Overall Experiences with New ARRIS Post Acquisition.txt")
texto31 <- readLines("filescustomerinsight/Perception of ARRIS.txt")
texto32 <- readLines("filescustomerinsight/How ARRIS Compares to Competitive Offerings-Alternatives.txt")
texto33 <- readLines("filescustomerinsight/How to Better Partner with You in the Future.txt")
texto34 <- readLines("filescustomerinsight/How to Modify Professional Services to Better Meet Needs.txt")
texto35 <- readLines("filescustomerinsight/What You Would Like to say to CEO of ARRIS.txt")




texto <- texto35






# remove punctuation
texto = gsub("[[:punct:]]", "", texto)
# remove numbers
texto = gsub("[[:digit:]]", "", texto)
# remove unnecessary spaces
texto = gsub("[ \t]{2,}", "", texto)
texto = gsub("^\\s+|\\s+$", "", texto)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}


# lower case using try.error with sapply 
texto = sapply(texto, try.error)

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
#------------------------------------------------------------------------------
#qplot(factor(sent_df$text), sent_df$emotion, geom="bar", stat="identity")
qplot(factor(sent_df$emotion))
qplot(factor(sent_df$polarity))
summary(factor(sent_df$polarity))
#------------------------------------------------------------------------------

# separating text by emotion or by polarity
#emos = levels(factor(sent_df$emotion))
emos = levels(factor(sent_df$polarity))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  #tmp = texto[emotion == emos[i]]
  tmp = texto[polarity == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm2 <- tdm
tdm = as.matrix(tdm)
colnames(tdm) = emos

#comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5)


#-----------------Term Frec--------------------------
myTdm <- tdm
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=2)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

#----------------------network graph ------------------
plot(tdm2,terms=findFreqTerms(tdm2, lowfreq=4)[1:9], corThreshold=0.8)

#---------------------------------------------

