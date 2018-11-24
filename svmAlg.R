getSVM <- function(dataset, testData, epoch, lambda, w, b){
  
trainingClassifier = dataset[,"DataLabel"]
dataset = dataset[-c(ncol(dataset))]
dataset=as.matrix(dataset)
testClassifier = testData[ncol(testData)]
testData = testData[-c(ncol(testData))]
testData = as.matrix(testData)
set.seed(1234)
# classifier steps / epochs
numStepsPerEpoch = 1000;
  
    # learning rate
    n = 1/ (0.01*epoch + 10)
    
    for (step in 1:numStepsPerEpoch){
      # select index randomly
      index = sample.int(nrow(dataset),1);
      
      xi = dataset[index, ]
      yi = trainingClassifier[index]
      value = yi * (t(w) %*%xi + b) #yk (w * xk + b) 
      
      if (value >= 1) {
        w = w - n*lambda*w
        b = b
      } else {
        w = w - n * (lambda*w - yi*xi)
        b = b + n*yi
      }
    
    }

estimate = testData %*% w + b
predicted = rep(0,length(testClassifier))
predicted [estimate < 0] = -1 ;
predicted [estimate >= 0] = 1 ;
correctPrediction = sum(predicted == testClassifier)

returnList <- list(correctPrediction, w, b)

returnList

}

    



























