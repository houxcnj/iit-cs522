install.packages("rjson")
install.packages("tm")
install.packages("svd")
install.packages("fpc")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("cluster")

library(rjson)
library(tm)
library(svd)
library(fpc)
library(wordcloud)
library(cluster)
library(RColorBrewer)
json_file <- "C:/Users/tofor_000/Desktop/Restaurant_Reviews/Restaurants_Reviews.json"


data <- UnpackJSON(json_file)
text <- UnlistJSON(t(data$text))
text <- t(text)
#text <- text[,1]
#text <- unlist(text)

corpus <- VCorpus(VectorSource(text),readerControl=list(reader=readPlain))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument) 
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

dtm <- DocumentTermMatrix(corpus,control=list(weight=weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.9)


q <-as.numeric(as.matrix(dtm[1,]))
result <- t(q)%*%Vk%*%scale(solve(diag(svd$d[1:5])))


findFreqTerms(dtm,200)

svd <- propack.svd(as.matrix(dtm),neig = 200)

Uk <- scale(svd$u[,1:5])
Vk <- scale(svd$v[,1:5])
Dk <- scale(diag(svd$d[1:5]))


Mk <- Uk%*%Dk%*%t(Vk)
#cl <- kmeans(dtm,50)
cl <- kmeans(Mk,50)
colour <- cbind("red","green","blue","purple","yellow","brown","black","orange","blue2","red2")
plot(cl$centers,pch=19,col=colour)
plotcluster(Mk,cl$cluster)

#plotcluster(Mk,cl$cluster)


wordlist <- svd$v[,]
rownames(wordlist) <- colnames(dtm)
wordlist.sorted <-sort(abs(wordlist[,25]), decreasing = TRUE)[1:30]
d <- data.frame(word = names(wordlist.sorted),freq=wordlist.sorted)

#CLOUD OF WORDS

pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=2400,height=1800)
wordcloud(d$word,d$freq, scale=c(1,20),min.freq=0.01,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

sortRestaurantbYID <- function(data,topic,dtm,svd,Vk){
  
#  topic <- 2
  rankings <- matrix(0,3000,1000000) 
  rankings [1,1]<- data$stars[1][[1]]
  j<-1
  k<-1
  
  for(i in 2:length(data$business_id)){
    
    if (as.character(data$business_id[i]) == as.character(data$business_id[(i-1)])){
      
      q <-as.numeric(as.matrix(dtm[i,]))
      result <- t(q)%*%Vk%*%scale(solve(diag(svd$d[1:5])))
      if(result[topic]>5){
      rankings[k,j] <- data$stars[i][[1]]
      }
      k<-k+1
      
    }
    else{
      j<-j+1
      k<-1
      q <-as.numeric(as.matrix(dtm[i,]))
      result <- t(q)%*%Vk%*%scale(solve(diag(svd$d[1:5])))
      if(result[topic]>5){
      rankings[k,j] <- data$stars[i][[1]]
      }
      k<-k+1
    }
  } 
  
  final<- vector()
  
  for(i in 1:dim(rankings)[2]){
    sum<-0
    number<-0
    for(j in 1:dim(rankings)[1]){
      
      if(rankings[j,i] != 0){
        sum <- sum + rankings[j,i]
        number<- number+1
      }
      final[i]<-sum/number
    }
    
  }
  return(final)
}

# Convert raw JSON file into managable data frame
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

# Convert the nested lists into regular vectors
UnlistJSON <- function(df) {
  
  for(i in 1:ncol(df)) {
    temp <- unlist(df[,i])
    names(temp) <- NULL
    df[,i] <- temp
  }
  return(df)
}

# Convert the votes column from a list into 3 seperate columns for useful, funny, cool
UnwrapVotes <- function(df) {
  temp <- unlist(df$votes)
  names(temp) <- NULL
  index <- seq(from=1, to=length(temp)-2, by=3)
  df$useful <- temp[index]
  index <- seq(from=2, to=length(temp)-1, by=3)
  df$funny <- temp[index]
  index <- seq(from=3, to=length(temp), by=3)
  df$cool <- temp[index]
  return(df)
}