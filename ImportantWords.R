#!/usr/bin/env Rscript
require(tm)
require(SnowballC)
require(wordcloud)


ImportantWords <- function(text, wordcloud=F, stem=F, df=F, commas=T) {
  ## text: the string of text you want to process
  ## wordcloud: T/F for generating a wordcloud
  ## stem: Stem the words (i.e. get the root)
  ## df: to return a data frame or just a string of the words
  ## commas: if df=F, to insert commas in between words instead of spaces

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


  if(wordcloud) wordcloud(words=wordsDF$word, freq=wordsDF$freq, min.freq=1,
                          max.words=200, random.order=FALSE)


  if(df) return(wordsDF)  
  else if(commas) return(paste(wordsDF$word, collapse=", "))
  else return(paste(wordsDF$word, collapse=" "))
}


fileName <- commandArgs(TRUE) #Allows the filename to be specified from Bash
keywords <- ImportantWords(readChar(fileName, file.info(fileName)$size), wordcloud=F)


print(keywords)
