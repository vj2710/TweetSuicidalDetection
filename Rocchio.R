source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\FeatureExtraction.R")
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\performanceEvaluation.R")
getPlot <- function(accuracy){
  
  accuracy_dataframe = as.data.frame(accuracy)
  K_Folds = 1:10
  qplot(K_Folds,accuracy, data=accuracy_dataframe,geom = c("point", "line","smooth"))
}

getConfMatColNames <- function(val){
  if(val == -1){
    return("suicide")
  }
  else{
    return("nonsuicide")
  }
}

classifyNewTweet <- function (tweetToBeClassified, calculatedMean) {
  class = 0
  suicidalDistance = 0
  nonSuicidalDistance = 0
  
  suicidalDistance = sum(tweetToBeClassified^2) + sum(calculatedMean[1,]^2) - 2* tweetToBeClassified %*% calculatedMean[1,]
  nonSuicidalDistance = sum(tweetToBeClassified^2) + sum(calculatedMean[2,]^2) - 2* tweetToBeClassified %*% calculatedMean[2,]
  if(nonSuicidalDistance<suicidalDistance){
    class = 1
  }
  else{
    class = -1
  }
  class
}

calculateMean <- function(tfidfMat){
  #calculating mean
  meanSuicidal = c()
  meanNonSuicidal = c()
  noOfSuicidalDocs = sum(dataset[,3]==-1)
  noOfNonSuicidalDocs = sum(dataset[,3]==1)
  for(j in 1:ncol(tfidfMat)){
    groupedClass = as.list(tapply(tfidfMat[,j], tfidfMat[,"DataLabel"], sum))
    meanSuicidal = c(meanSuicidal,groupedClass[["-1"]]/noOfSuicidalDocs)
    meanNonSuicidal = c(meanNonSuicidal,groupedClass[["1"]]/noOfNonSuicidalDocs)
  }
  calculatedMean = data.frame()
  calculatedMean = rbind(calculatedMean, meanSuicidal)
  calculatedMean = rbind(calculatedMean, meanNonSuicidal)
  
  calculatedMean
}


#Reading from dataset
dataset <<- read.csv('Twitter_Suicide_Data_new.csv', header = TRUE, stringsAsFactors = FALSE)


#to shuffle the rows
shuffleIndex = sample(1:nrow(dataset)) 
dataset <<- dataset[shuffleIndex, ]
tfidfData = extractFeatures(dataset)


datasetSize <<- nrow(dataset)
headers = c("suicide", "nonsuicide")
confusionMat = matrix(c(0:0), nrow = length(headers), ncol = length(headers), byrow = TRUE, dimnames = list(headers, headers))

tenFoldRows <<- trunc(datasetSize/10)
testSetStart <<- 0
accuracy = c()
truePrediction = 0
trainingSet = c()
testSet = c()

for (i in 1:10){
  #Cross-validation
  testSetEnd <<- testSetStart + tenFoldRows
  testSet = na.omit(tfidfData[testSetStart:testSetEnd,])
  if(testSetEnd != datasetSize){
    trainingSet = na.omit(tfidfData[c(0:testSetStart, testSetEnd+1:datasetSize), ])  
  }
  else
  {
    trainingSet = na.omit(tfidfData[0:testSetStart])  
    
  }
  calculatedMean = calculateMean(trainingSet)
  
  truePrediction = 0
  #code for rochhio starts
  for(j in 1:nrow(testSet)){
    estimate = classifyNewTweet(as.matrix(testSet[j,1:(ncol(testSet)-1)]), as.matrix(calculatedMean[,1:(ncol(calculatedMean)-1)]))
    if(estimate == testSet[j, ncol(testSet)]){
      truePrediction = truePrediction + 1
    }
    estimate = getConfMatColNames(estimate)
    actual = getConfMatColNames(testSet[j, ncol(testSet)])
    confusionMat[estimate, actual] = confusionMat[estimate, actual] + 1 
  }
  accuracy[i] = (truePrediction/nrow(testSet))*100
  testSetStart <<- testSetEnd
}

evaluatePerformace(confusionMat)
print(accuracy)
getPlot(accuracy)
