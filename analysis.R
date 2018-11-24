
getAnalytics <- function(confusionMat){
  analysis <- list("accuracy"="","performanceAnalysis"="")
  confusionMatAnalysis <- matrix(nrow = 5, ncol = ncol(confusionMat))
  colnames(confusionMatAnalysis) <- colnames(confusionMat)
  rownames(confusionMatAnalysis) <- c("precision","reCall","falsePositiveRate","falseNegativeRate","specificity")
  for(featureName in colnames(confusionMatAnalysis)){
    confusionMatAnalysis["precision",featureName] <- precision(confusionMat,featureName)*100
    confusionMatAnalysis["reCall",featureName] <- truePositveRate(confusionMat,featureName)*100
    confusionMatAnalysis["falsePositiveRate",featureName]  <- falsePositiveRate(confusionMat,featureName)*100
    confusionMatAnalysis["falseNegativeRate",featureName]  <- falseNegativeRate(confusionMat,featureName)*100
    confusionMatAnalysis["specificity",featureName]  <- trueNegativeRate(confusionMat,featureName)*100
  }
  analysis$accuracy = accuracy(confusionMat)*100
  analysis$performanceAnalysis = confusionMatAnalysis
  analysis
}


truePositive <- function(confusionMat,feature){
  confusionMat[feature,feature]
}

falsePositive <- function(confusionMat,feature){
  sum(confusionMat[feature,]) - confusionMat[feature,feature]
}

trueNegative <- function(confusionMat,feature){
  sum(diag(confusionMat)) - truePositive(confusionMat,feature)
}

falseNegative <- function(confusionMat,feature){
  sum(confusionMat[,feature]) -confusionMat[feature,feature]
}


# sensitivity, recall, hit rate, or true positive rate (TPR)
truePositveRate <- function(confusionMat,feature){
  truePositive(confusionMat,feature)/(truePositive(confusionMat,feature)+falseNegative(confusionMat,feature))
}

# miss rate or false negative rate (FNR)
falseNegativeRate <- function(confusionMat,feature){
  falseNegative(confusionMat,feature)/(truePositive(confusionMat,feature)+falseNegative(confusionMat,feature))
}

# fall-out or false positive rate (FPR)
falsePositiveRate <- function(confusionMat,feature){
  falsePositive(confusionMat,feature)/(trueNegative(confusionMat,feature)+falsePositive(confusionMat,feature))
  
}

# precision or positive predictive value (PPV)
precision <- function(confusionMat,feature){
  truePositive(confusionMat,feature)/(truePositive(confusionMat,feature)+falsePositive(confusionMat,feature))
  
}

# specificity, selectivity or true negative rate (TNR)
trueNegativeRate <- function(confusionMat,feature){
  trueNegative(confusionMat,feature)/(trueNegative(confusionMat,feature)+falsePositive(confusionMat,feature))
}

accuracy <- function(confusionMat){
  sum(diag(confusionMat))/sum(confusionMat)
}
