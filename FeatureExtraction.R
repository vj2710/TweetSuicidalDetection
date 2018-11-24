
library(tm)

cleanTweet <- function (tweets) {
  
  # remove retweet entities
  cleanedTweet = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', ' ', tweets)
  # remove at people
  cleanedTweet = gsub('@\\w+', ' ', cleanedTweet)
  # remove punctuation
  cleanedTweet = gsub('[[:punct:]]', ' ', cleanedTweet)
  # remove numbers
  cleanedTweet = gsub('[[:digit:]]', ' ', cleanedTweet)
  # remove html links
  cleanedTweet = gsub('http\\w+', ' ', cleanedTweet)
  #remove enter
  cleanedTweet = gsub("[\r\n]", ' ', cleanedTweet)
  # remove emojis or special characters
  cleanedTweet = gsub('<.*>', ' ', enc2native(cleanedTweet))
  # remove stop words
  cleanedTweet = removeWords(cleanedTweet, stopwords("en"))
  # stemming
  cleanedTweet = stemDocument(cleanedTweet)
  # remove unnecessary spaces
  cleanedTweet = gsub('[ \t]{2,}', '', cleanedTweet)
  cleanedTweet = gsub('^\\s+|\\s+$', '', cleanedTweet)
  
  cleanedTweet = tolower(cleanedTweet)
  
  cleanedTweet
}

extractFeatures <- function(dataset){
  #Reading from dataset
  #dataset = read.csv('Complete data set.csv', header = TRUE, stringsAsFactors = FALSE)
  noOfDocs <<- nrow(dataset)
  # to remove non-ascii
  Encoding(dataset[,2]) <- "latin1" 
  dataset[, 2] = iconv(dataset[,2], "latin1", "ASCII", sub="")
  
  #to clean the tweets
  for(i in 1:noOfDocs){
    dataset[i,2] = cleanTweet(dataset[i,2])
  }
  
  
  myFactor = {}
  myVector = c()
  for(tweet in dataset[, 2]){
    for(word in strsplit(tweet, "\\s+")){
      myVector = c(myVector, word)
    }
  }
  
  # overall frequencies of words
  myVector = factor(myVector)
  df = data.frame(table(myVector))
  
  myFrame <- data.frame(matrix(0, ncol = nrow(df), nrow = noOfDocs))
  colnames(myFrame) <- df[, 1]
  
  myFrame = myFrame[, order(names(myFrame))]
  
  counter = 0
  for(tweet in dataset[, 2]){
    counter = counter + 1
    words = strsplit(tweet, "\\s+")[[1]]
    for(word in words){
      myFrame[counter, word] = myFrame[counter, word]+1
    }
  }
  
  #removing sparse features having density less than 1% 
  sumMatrix = c()
  sumMatrix = nrow(myFrame) - colSums(myFrame==0) 
  removeIndex = c()
  sparsityFactor = 0.01 * noOfDocs
  
  for(i in 1:length(sumMatrix)){
    if(sumMatrix[i] < sparsityFactor) {
      removeIndex = c(removeIndex, i)
    }
  }
  
  #to remove the sparse features
  myFrame = myFrame[, -c(removeIndex)]
  
  # write.csv(myFrame, "C:/Users/jainv/Google Drive/Data Mining/project data set/mydata22.csv")
  
  #creating the tf-idf matrix
  tfidfMat = data.frame(myFrame)
  for(i in 1:nrow(myFrame)){
    totalWordsInDoc = sum(myFrame[i,])
    for(j in 1:ncol(myFrame)){
      totalDocsHavingWord = sum(myFrame[,j]!=0)
      if(totalWordsInDoc!=0){
        tf = myFrame[i,j]/totalWordsInDoc
      }
      idf = log10(noOfDocs/totalDocsHavingWord)
      tfidfMat[i,j]=tf*idf
    }
  }

  #adding label to the matrix
  tfidfMat$DataLabel=dataset[,3]

  tfidfMat

}





