require(tm)
require(purrr)
require(SnowballC)
require(wordcloud)


ImportantWords <- function(text, wordcloud=F, stem=F) {
  textC <- Corpus(VectorSource(text))

  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  textC <- tm_map(textC, toSpace, "/")
  textC <- tm_map(textC, toSpace, "@")
  #textC <- tm_map(textC, toSpace, "\\|")

  textC <- tm_map(textC, content_transformer(tolower))
  textC <- tm_map(textC, removeNumbers)
  textC <- tm_map(textC, removeWords, stopwords("english"))
  textC <- tm_map(textC, removePunctuation)

  textC <- tm_map(textC, stripWhitespace)

  # Only stem words if you specifically ask for it.
  if (stem) textC <- tm_map(textC, stemDocument)

  docM <- TermDocumentMatrix(textC)

  sorted <- sort(rowSums(as.matrix(docM)),decreasing=TRUE)
  wordsDF <- data.frame(word = names(sorted),freq=sorted,row.names=NULL)

  if(wordcloud) {
    wordcloud(words=wordsDF$word, freq=wordsDF$freq, min.freq=1,
                          max.words=200, random.order=FALSE)
}
  return(wordsDF)


}



fileName <- 'Descriptions/***'
keywords <- ImportantWords(readChar(fileName, file.info(fileName)$size), wordcloud=T)


print(paste(keywords$word, collapse=" "))
