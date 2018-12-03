source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\naive_bayes.R")

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

convertResult <- function(results_training ){
  results_training[,ncol(results_training)] <- as.integer(results_training[,ncol(results_training)])
  
  results_training <- data.frame(
    rownames(results_training),
    c(as.data.frame(results_training)[[1]]),
    c(as.data.frame(results_training)[[2]]),
    c(as.data.frame(results_training)[[3]])
  )
  
  newColNames = c("Rownames", "-1", "1", "i")
  colnames(results_training) = newColNames
  return(results_training)
}
#read the dataset
dataset <- read.csv('Twitter_Suicide_Data_new.csv', header = TRUE, stringsAsFactors = FALSE)
#to shuffle the rows
tfidfData = extractFeatures(dataset)
shuffleIndex = sample(1:nrow(dataset)) 
dataset <<- dataset[shuffleIndex, ]



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
  else{
    trainingSet = na.omit(tfidfData[0:testSetStart])  
  }
  results_training <- My_NaiveBayes(training_dataset)
  results_training = convertResult(results_training)
  headers = c("suicide", "nonsuicide")
  confusionMat = matrix(c(0:0), nrow = length(headers), ncol = length(headers), byrow = TRUE, dimnames = list(headers, headers))
  truePrediction = 0  
  for(j in 1:nrow(testSet)){
    estimate <- predict(testSet[j,-ncol(testSet)], results_training)
    if(estimate == testSet[j,ncol(testSet)]){
      truePrediction = truePrediction + 1
    }
    estimate = getConfMatColNames(estimate)
    actual = getConfMatColNames(testSet[j, ncol(testSet)])
    
    confusionMat[estimate, actual] = confusionMat[estimate, actual] + 1 
  }
  accuracy[i] = (truePrediction/nrow(testSet))*100
  testSetStart <<- testSetEnd
}
print(confusionMat)
print(accuracy)
evaluatePerformace(confusionMat)

getPlot(accuracy)





