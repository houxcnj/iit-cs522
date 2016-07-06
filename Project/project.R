rm(list=ls())

library("rjson")
library("NLP")
library("tm")
library("class")
library("slam")


UnpackJSON <- function(filePath) {
  con <- file(filePath, "r")
  input <- readLines(con, -1L)
  jsonData <- sapply(input,fromJSON)
  close(con)
  df <- data.frame(jsonData)
  temp <- rownames(df)
  df <- as.data.frame(t(df))
  colnames(df) <- temp
  rownames(df) <- NULL
  return(df)
}

reviewdata <- UnpackJSON("~/Downloads/yelp_dataset_challenge_academic_dataset/reviewaa.json")

text <- reviewdata["text"]
text <- as.matrix(text)

textc <- Corpus(VectorSource(text), readerControl = list(reader = readPlain))

textc <- tm_map(textc, stripWhitespace)
textc <- tm_map(textc, content_transformer(tolower))
textc <- tm_map(textc, removeWords, stopwords("english"))
textc <- tm_map(textc,removePunctuation)
textc <- tm_map(textc, removeNumbers)
textc <- tm_map(textc, stemDocument)

reviewdtm <- DocumentTermMatrix(textc)
reviewdtm.tfidf <- tapply(reviewdtm$v/row_sums(reviewdtm)[reviewdtm$i], reviewdtm$j, mean) * log2(nDocs(reviewdtm)/col_sums(reviewdtm > 0))
summary(reviewdtm.tfidf)
reviewdtm.new1 <- reviewdtm[,reviewdtm.tfidf >= 0.1]
reviewdtm.new1 <- reviewdtm.new1[row_sums(reviewdtm.new1) > 0,]
summary(col_sums(reviewdtm.new1))

reviewdtm <- removeSparseTerms(reviewdtm, 0.95)
rowTotal <- apply(reviewdtm, 1, sum)
reviewdtm.new <- reviewdtm[rowTotal>0, ]
hist(apply(reviewdtm.new, 1 , sum),xlab="Number of Terms in Term-Document Matrix", main="Number of Terms Per Review Corpus", breaks=100)

library(SparseM)
library(RTextTools)
library(topicmodels)
library(lda)
library(ggplot2)
library(reshape2)

#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 100, by = 1), function(d){LDA(reviewdtm.new, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
plot(2:100, best.model.logLik$V1, type = "l", xlab = "kernels", ylab = "LogLik")

k <- 5
SEED <- 1000

reviewTM <-
  list(VEM = LDA(reviewdtm.new, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(reviewdtm.new, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(reviewdtm.new, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(reviewdtm.new, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3)))
       )

ldadata <- lexicalize(textc)
keep <- ldadata$vocab[word.counts(ldadata$documents, ldadata$vocab) >= 3]
ldadata1 <- lexicalize(textc, lower=TRUE, vocab=keep)

result <- lda.collapsed.gibbs.sampler(ldadata$documents,k,ldadata$vocab,25,0.1,0.1)

top.words <- top.topic.words(result$topics, 20, by.score=TRUE)
top.words

N <- 10 #Choose the number of documents to display
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <- topic.proportions[sample(1:dim(topic.proportions)[1], N,replace = TRUE), ]
topic.proportions[is.na(topic.proportions)] <-  1 / k
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable_name="topic",
                             id.vars = "document")
qplot(, value, fill=document, ylab="proportion",
      data=topic.proportions.df, geom="bar") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip() +
  facet_wrap(~ document, ncol=5)
#To compare the fitted models we first investigate the values of the models fitted with VEM and estimated and with VEM and fixed 
sapply(reviewTM[1:2], slot, "alpha")
sapply(reviewTM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))


Topic <- topics(reviewTM[["VEM"]], 1)

#top 5 terms for each topic in LDA
Terms <- terms(reviewTM[["VEM"]], 20)
Terms

my_topics <- topics(reviewTM[["VEM"]])

most_frequent <- which.max(tabulate(my_topics))

terms(reviewTM[["VEM"]], 10)[, most_frequent]

library ("ggplot2")
qplot(Topic, value, fill=document, ylab="proportion",
      data=topic.proportions.df, geom="bar") +
  opts(axis.text.x = theme_text(angle=90, hjust=1)) +
  coord_flip() +
  facet_wrap(~ document, ncol=5)

reviewTopicma <- matrix(, nrow = 2000, ncol = 5)
for (i in 1:length(reviewdata$text)) {
  reviewTopicma[i,Topic[i]] = reviewdata$stars[i][[1]]
}
first <- 1
last <- 1
buzRam <- c()
for (i in 2: length(reviewdata$business_id)) {
  if (as.character(reviewdata$business_id[i]) == as.character(reviewdata$business_id[(i-1)])){
    }
  else {
    last = i-1
    subRT <- reviewTopicma[first:last,  , drop = FALSE]
    buzRating <- colMeans (subRT, na.rm = TRUE, dims = 1)
    dim(buzRating) <- c(1,5)
    rownames(buzRating) <- as.character(reviewdata$business_id[(i-1)])
    buzRam <- rbind(buzRam, buzRating)
    first = i
  }
}

colnames(buzRam) <- c("fast and taste", "bar and drink", "worth and menu", "health and service","together and easy-order")

reviewdtm.new1 <- weightTfIdf(reviewdtm.new)

findFreqTerms(reviewdtm.new1, lowfreq=50)

library (RColorBrewer)
library ("wordcloud")
m = as.matrix(reviewdtm.new1);
v = sort(colSums(m), decreasing=TRUE);
myNames = names(v);
k = which(names(v)=="miners");
myNames[k] = "mining";
d = data.frame(word=myNames, freq=v);
wordcloud(d$word, colors=c(3,4), random.color=FALSE, d$freq, min.freq=50);

