
#----Finding unique values for a column
unique_vals <- function(rows,col){
     return (unique(rows[,col]))
}


class_counts<- function(rows){
  rows <- as.data.frame(rows)
  num_feature = length(rows[,-ncol(rows)])
  for (i in 1:nrow(rows)){ 
       lbl = rows[i,num_feature+1]
       
       if (is.na(lbl) || length(lbl)==0)
       {
         next
       }
       else
      { label_matrix[1,lbl]<- label_matrix[1,lbl]+1
      }
  }
  return(label_matrix)
}

check_numeric <- function(input) {
  
  return (is.numeric(input) || is.integer(input) || is.double(input))
}

partition <- function(example,feature,splitValue) {
  mask <- example[,feature] < splitValue
  left_child <- example[mask,]
  right_child <- example[!mask,]
  
  list_data <- list(left_child,right_child)
  return(list_data)
  
}

#Calculate the Gini Impurity for a list of rows.
gini <- function(rows) {
  
  #feature_data = rows[,ncol(rows)]
  #if(length(feature_data) == 0) 
    #return(0)
  
  #return(1- sum((table(feature_data)/length(feature_data))^2)) 
  
   
   counts=class_counts(rows)
   impurity=1
   for (lbl in colnames(counts)){
     prob_of_lbl = unname(counts[1,lbl],force=FALSE) / as.double(nrow(rows))
     impurity = impurity - (prob_of_lbl**2)
   }
   
   return (impurity)
}

#Information Gain.
info_gain <- function(left,right,current_uncertainty) {
  
  p = as.double(nrow(left) / (nrow(left) + nrow(right)))
  return (current_uncertainty - p * gini(left) - (1 - p) * gini(right))
}

find_best_split <- function(rows){
  
  best_gain = 0  # keep track of the best information gain
  best_question = "-1"  # keep train of the feature / value that produced it
  current_uncertainty = gini(rows)
  rows <- as.data.frame(rows)
  num_feature = length(rows[,-ncol(rows)])
  for (col in 1:num_feature){
    best_gain = -1
    best_question = -1
    best_value=-1
   # if(unique(rows))
   
    for (val in  unique(rows[,col])) {
      #question = Question(dataset,col, val,"<")
      question= (paste("Is ",colnames(as.data.frame(rows))[col]," <",val," ?"))
      is_numeric = !(is.factor(rows[,col])|is.logical(rows[,col])|is.character(rows[,col]))
      #print(question)
      partition_result <-  partition(rows,col, val)
      true_data <- partition_result[[1]]
      false_data <- partition_result[[2]]
      # Skip this split if it doesn't divide the
      # dataset.
     
      if((nrow(true_data)==0) || (nrow(false_data)==0) )
      {
        next
      }
      else
      {
        # Calculate the information gain from this split
        gain = info_gain(true_data, false_data, current_uncertainty)
        #print(question[1])
        #print(true_data)
        #print(false_data)
     
        if (gain > best_gain)
        {
          best_gain=gain
          best_question=question
          best_value=val
        }
      }
    }
    if (col==1){
      var <- data.frame(best_gain,best_col_name=names(rows)[col],question=best_question[1],split_value=best_value,is_numeric,col_idx=col)
    }
    else
    {
      updated_var <- data.frame(best_gain,best_col_name=names(rows)[col],question=best_question[1],split_value=best_value,is_numeric,col_idx=col)
      var <- rbind(var,updated_var)
    }
    
  }
  
  return(var)
}



#selecting the best split Question
best_feature_split <- function(X){
  best_name <- X[(which.max(X[,'best_gain'])),'best_col_name']
  best_result <- X[which.max(X[,'best_gain']),]
  best_result<- as.data.frame(best_result)
  #print(best_name)
  #print(best_result)
  return(best_result)
}


build_tree <- function(TreeNode){
 
  #TreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
  #TreeNode$dataset <- dataset 
  #TreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
  #TreeNode$dataset <- dataset
   #TreeNode$dataset <- false_data
  all_feature_best_split_results <- find_best_split(TreeNode$dataset)
  TreeNode$bestSplit<- best_feature_split(all_feature_best_split_results)
  TreeNode$info_gain <- TreeNode$bestSplit$best_gain
  
  
  
  if(TreeNode$info_gain > 0 && nrow(TreeNode$dataset) > 20){
    
    col <- strtoi(TreeNode$bestSplit[,'col_idx'])[1]
    val <- TreeNode$bestSplit$split_value
    partition_result <- partition(TreeNode$dataset,col,val)
    print("***************8Spilt success")
    true_data <- partition_result[[1]]
    false_data <-  partition_result[[2]]
    if(nrow(true_data)<5 || nrow(false_data)<5){
      return(TreeNode)
    }
    colnames(true_data) <- header
    colnames(false_data) <- header
    print(TreeNode$bestSplit)
    print(nrow(TreeNode$dataset))
    print("after split***************")
    print(nrow(true_data))
    print(nrow(false_data))
    
    leftTreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
    leftTreeNode$dataset <- true_data
    #TreeNode$leftchild <- leftTreeNode  
    
    rightTreeNode <-list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
    rightTreeNode$dataset <- false_data
    #TreeNode$rightchild <- rightTreeNode
    
    TreeNode$leftChild <- build_tree(leftTreeNode) 
    TreeNode$rightChild <- build_tree(rightTreeNode)
    
    
    return (TreeNode)
  }
  else 
  {
    return(TreeNode)
  }
 
 
}


#predicting class for a test row
prediction <- function(node,test_row) {
  
  if( ((node$leftChild=="NULL") || (node$rightChild=="NULL") ))
  { 
    return (tail(names(sort(table(node$dataset$DataLabel))),1))
  }
  else
  {
  
    feature_col <- strtoi(node$bestSplit[,'col_idx'])[1]
    feature_split_value <- as.numeric(as.character(node$bestSplit$split_value))
    test_row_feature_value <- test_row[1,feature_col]
    
      if(test_row_feature_value < feature_split_value) #go to left child
    {
      left <- node$leftChild
      return (prediction(left,test_row))
    }
    else(test_row_feature_value >= feature_split_value) #go to right child
    {
      
      right <- node$rightChild
      return(prediction(right,test_row))
    }
  }
}

#looping over the dataset and predicting the class using our decision tree

# predictDataSetClass <- function(decisionTreeNode,testData){
#   for (row in 1:length(testData[,num_feature+1])){
#     test_row <-  testData[row,]
#     resultTestClass$PredictedClass[row] <- prediction(decisionTreeNode,test_row)
#     
#   }
#   return (resultTestClass)
# }


runAlgo <- function(dataset){
  num_feature <<- length(dataset[,-(ncol(dataset))])
  header <<- colnames(dataset)
  class_uniq <- unique_vals(dataset,(num_feature)+1)
  #-----Counts the number of each type of example in a dataset and making a matrix of the class label and initialize it to 0
  for(i in 1:length(class_uniq)) {
    
    if(i==1){
      label_vec<- c(0)
    }
    else
    {
      label_vec <- c(label_vec,0)
    }
  }
  
  label_matrix <<- matrix(c(label_vec),ncol=length(class_uniq))
  colnames(label_matrix) <<- class_uniq
  
  rootTreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
  rootTreeNode$dataset <- dataset 
  resultant <- build_tree(rootTreeNode) 
  resultant
}
