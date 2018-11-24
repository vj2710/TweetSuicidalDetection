evaluatePerformace <- function(confusionMat){
  listOfRows <- c('Precision', 'Recall', 'Specificity', 'F1 score')
  performanceMat <- data.frame(row.names = listOfRows)
  
  Precision = (confusionMat[1,1]/(confusionMat[1,1]+confusionMat[1,2]))*100
  Recall= (confusionMat[1,1]/(confusionMat[1,1]+confusionMat[2,1]))*100
  Specificity =  (confusionMat[2,2]/(confusionMat[2,2]+confusionMat[1,2]))*100
  F1score = 2 * ((Precision * Recall)/(Precision + Recall))
  
  performanceMatrix = data.frame(Value=c(Precision, Recall, Specificity, F1score),
                                 stringsAsFactors = FALSE)
  rownames(performanceMatrix) = c("Precision", "Recall", "Specificity", "F1Score")
  
  print(performanceMatrix)
}


