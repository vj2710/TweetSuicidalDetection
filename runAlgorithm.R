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

runAlgorithm <- function(dataset){
  print("SVM")
  datasetSize <<- trunc(nrow(dataset) * (90/100))
  validationSet = as.matrix(dataset[(datasetSize+1):nrow(dataset),])
  dataset = dataset[1:datasetSize,]
  tenFoldRows <<- trunc(datasetSize/10)
  bestW=rep(0, ncol(dataset));
  bestB=0
  bestCorrectCount = -1
  
  lambdaSVM = c(0.001, 0.01, 0.1, 1)
  for(k in 1:length(lambdaSVM)){
  testSetStart <<- 0
  lambdaCorrectCount = 0
  trainingSet = c()
  testSet = c()
  
    w = rep(0, (ncol(dataset)-1))
    b = 0
    
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


    returnList = getSVM(dataset, testSet, i, lambdaSVM[k], w, b)
    lambdaCorrectCount = lambdaCorrectCount + returnList[[1]]
    w = returnList[[2]]
    b = returnList[[3]]
    
    testSetStart <<- testSetEnd
    }
    if(lambdaCorrectCount>bestCorrectCount){
      bestCorrectCount = lambdaCorrectCount
      bestW=w
      bestB=b
      print(lambdaSVM[k])
    }
  }
  
  headers = c("suicide", "nonsuicide")
  confusionMat = matrix(c(0:0), nrow = length(headers), ncol = length(headers), byrow = TRUE, dimnames = list(headers, headers))
  truePrediction = 0  
  for(i in 1:nrow(validationSet)){
    estimate = validationSet[i,1:(ncol(validationSet)-1)] %*% bestW + bestB;
    print(estimate)
    if(estimate<0){
      estimate = -1
    }
    else{
      estimate = 1
    }
    if(estimate == validationSet[i,ncol(validationSet)]){
      truePrediction = truePrediction + 1
    }
    
    estimate = getConfMatColNames(estimate)
    actual = getConfMatColNames(validationSet[i, ncol(validationSet)])
    confusionMat[estimate, actual] = confusionMat[estimate, actual] + 1 
   
  }
  Accuracy = (truePrediction/nrow(validationSet))*100
    print(confusionMat)
    print(Accuracy)
    evaluatePerformace(confusionMat)
  
}


