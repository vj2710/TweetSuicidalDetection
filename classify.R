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