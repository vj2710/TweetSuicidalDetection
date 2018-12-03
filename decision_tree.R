getPlot <- function(accuracy){
  
  accuracy_dataframe = as.data.frame(accuracy)
  K_Folds = 1:10
  qplot(accuracy,K_Folds, data=accuracy_dataframe,geom = c("point", "line","smooth"))
}

getConfMatColNames <- function(val){
  if(val == -1){
    return("suicide")
  }
  else{
    return("nonsuicide")
  }
}

runDecisionTree <- function(dataset){
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
    resultant <- runAlgo(trainingSet)
    
    truePrediction = 0
    #code for rochhio starts
    for(j in 1:nrow(testSet)){
      estimate = prediction(resultant, testSet[j,])
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
  
  print(accuracy)
  print(confusionMat)
  evaluatePerformace(confusionMat)
  getPlot(accuracy)
}

