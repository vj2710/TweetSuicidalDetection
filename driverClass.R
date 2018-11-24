#Reading from dataset
dataset <<- read.csv('Twitter_Suicide_Data_new.csv', header = TRUE, stringsAsFactors = FALSE)


tfidfData = extractFeatures(dataset)
#to shuffle the rows
shuffleIndex = sample(1:nrow(dataset)) 
tfidfData <<- tfidfData[shuffleIndex, ]


runAlgorithm(tfidfData)