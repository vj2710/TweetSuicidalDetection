#Reading from dataset
setwd("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\Datasets")
dataset <<- read.csv('Twitter_Suicide_Data_new.csv', header = TRUE, stringsAsFactors = FALSE)
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\FeatureExtraction.R")
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\performanceEvaluation.R")
tfidfData = extractFeatures(dataset)
#to shuffle the rows
shuffleIndex = sample(1:nrow(dataset)) 
tfidfData <<- tfidfData[shuffleIndex, ]

#SVM
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\runAlgorithm.R")
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\svmAlg.R")
runAlgorithm(tfidfData)

#to run decision tree
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\decision_ALGO.R")
source("C:\\Users\\jainv\\Desktop\\Suicidal Detection\\decision_tree.R")
runDecisionTree(round(tfidfData, 5))












