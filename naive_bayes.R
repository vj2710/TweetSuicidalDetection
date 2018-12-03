My_NaiveBayes<- function (dataset){
  
  num_feature = ncol(dataset) -1
  num_obs = nrow(dataset)
  num_class=length(unique(dataset[,num_feature+1]))
 
      #the conditional probilities -->
      for (i in 1:num_feature){
        tab <- table(dataset[,i],dataset[,num_feature+1])
        #colnames(dataset)[1]
        #tab = cbind(tab,rep(colnames(dataset)[i]))
        if(i==1) {
          tab_col_sum <- apply(tab,2,sum)
          cond_prob <- sweep(tab,2,tab_col_sum,"/")
          cond_prob = cbind(cond_prob,i)
        }
        else{
          tab_col_sum <- apply(tab,2,sum)
          cond_prob_updates <- sweep(tab,2,tab_col_sum,"/")
          cond_prob_updates = cbind(cond_prob_updates,i)
          cond_prob <- rbind(cond_prob,cond_prob_updates)
        }
      }
  
      tab_col_sum = tab_col_sum/sum(tab_col_sum)
      length(tab_col_sum) <- max(length(tab_col_sum), ncol(cond_prob))
      tab_col_sum[length(tab_col_sum)] <- -2
      cond_prob_final = rbind(tab_col_sum,cond_prob)
      
      return (cond_prob_final)
}

predict <- function(test_row,cond_prob_final){
      
      cond_prob_final[,ncol(cond_prob_final)] <- as.integer(cond_prob_final[,ncol(cond_prob_final)])
      num_feature <- ncol(test_row)
      pred = names(which.max(cond_prob_final[1,2:3]))
      # prediction --->
       if(ncol(cond_prob_final) > 3) #More than two class in training -> need to fix!
      {
         removeIndex=c()
         for(i in 1:length(test_row)){
          # if(!(test_row[i]  %in% rownames(cond_prob_final))){
           if(test_row[i] %in% cond_prob_final$Rownames == FALSE){
             removeIndex = c(removeIndex, i)
            
           }
         }
         if(length(removeIndex)>0){
           test_row = test_row[-c(removeIndex)]
         }
         
         if(length(test_row)>0){
           obs <- cond_prob_final[cond_prob_final$Rownames %in% c(test_row[1:length(test_row)]),]
           
           
           obs1 <- as.data.frame(obs)
           obsFinal = c()
           for(j in 1:length(test_row)){
             temp = obs1[obs1$Rownames %in% test_row[,j],]
             obsFinal = rbind.data.frame(obsFinal,temp[temp$i==j,])  
           }
           obsFinal = rbind.data.frame(ClassProbabilities=cond_prob_final[1,],obsFinal)
           obsFinal[,2][obsFinal[,2]==0]<-0.0001
           obsFinal[,3][obsFinal[,3]==0]<-0.0001
           obsprob=apply(obsFinal[,2:3],2,prod)
           pred <- names(which.max(obsprob[1:length(obsprob)]))
         }
         
    
       }
      
      else #only one class in the dataset - unbalanced dataset
        {
          pred <- unique(dataset[,num_feature+1])
        }
      
      #print(pred)
      return(pred)
}

