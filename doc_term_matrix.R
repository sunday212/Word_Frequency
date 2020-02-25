######## Word Frequency Count ######## 
##### using Document Term Matrix ##### 

wordcount<-function(filename,outputfile){
  
  eng<-read.csv(filename,stringsAsFactors = FALSE)
  
  content<-as.list(eng$content)		
  
  library(tm)
  
  docs <- Corpus(VectorSource(content))
  docs <-tm_map(docs,content_transformer(tolower))
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  docs <- tm_map(docs, toSpace, "-")
  docs <- tm_map(docs, toSpace, "'")
  docs <- tm_map(docs, toSpace, "'")
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "https")
  docs <- tm_map(docs, toSpace, "amp")
  docs <- tm_map(docs, toSpace, "t.co")
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs,stemDocument)
  
  dtm <- DocumentTermMatrix(docs)
  
  dtm = removeSparseTerms(dtm,0.999)
  
  raw.sum=apply(dtm,1,FUN=sum)
  dtm=dtm[raw.sum!=0,]
  
  
  #--------   MAKE LIST OF MOST COMMON WORDS  -----------
  
  freq <- colSums(as.matrix(dtm))
  length(freq)
  ord <- order(freq,decreasing=TRUE)
  list<-as.data.frame(freq[ord])
  
  write.csv(list,outputfile)
  View(list)
  
}