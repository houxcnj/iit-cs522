library("rjson")
library("NLP")
library("tm")
library("class")


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

#change to your own dirctory
reviewdata <- UnpackJSON("~/Downloads/yelp_dataset_challenge_academic_dataset/reivewab.json")

text <- reviewdata$text

textc <- Corpus(VectorSource(text), readerControl = list(reader = readPlain))

textc <- tm_map(textc, stripWhitespace)
textc <- tm_map(textc, content_transformer(tolower))
textc <- tm_map(textc, removeWords, stopwords("english"))
textc <- tm_map(textc,removePunctuation)
textc <- tm_map(textc, removeNumbers)
textc <- tm_map(textc, stemDocument)

reviewdtm <- DocumentTermMatrix(textc)
#reviewdtm <- weightTfIdf(reviewdtm)
reviewdtm <- removeSparseTerms(reviewdtm, 0.95)
rowTotal <- apply(reviewdtm, 1, sum)
reviewdtm.new <- reviewdtm[rowTotal>0, ]

findFreqTerms(reviewdtm.new, lowfreq=50)

#Do LDA
library(SparseM)
library(RTextTools)
library(topicmodels)
library(lda)
k <- 10
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
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

#you can change "VEM_fixed" to "VEM", "Gibbs" or "CTM" to see the difference
Topic <- topics(reviewTM[["VEM_fixed"]], 1)

#top 5 terms for each topic in LDA
Terms <- terms(reviewTM[["VEM_fixed"]], 20)
Terms

my_topics <- topics(reviewTM[["VEM_fixed"]])

most_frequent <- which.max(tabulate(my_topics))

terms(reviewTM[["VEM_fixed"]], 10)[, most_frequent]


reviewTopicma <- matrix(, nrow = 100000, ncol = 5)
for (i in 1:length(reviewdata$text)) {
    reviewTopicma[i,Topic[i]] = reviewdata$stars[i][[1]]
}

buzRank <- matrix (, nrow = 50000, ncol = 5)
for (i in 1: length(reviewdata$business_id)) {
    if (as.character(data$business_id[i]) == as.character(data$business_id[(i-1)])){
        
    }
}
