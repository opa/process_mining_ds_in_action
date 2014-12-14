# Quality measures


quality.confusion_matrix <- function( true_pos, false_pos, false_neg, true_neg ) {
  positive = true_pos + false_neg
  negative = true_neg + false_pos
  pos_prime = true_pos + false_pos
  neg_prime = true_neg + false_neg
  k = false_neg + true_neg + false_pos + true_neg
  error = (false_pos + false_neg) / k 
  accuracy = (true_pos + true_neg) / k 
  precision = true_pos / ( true_pos + false_pos ) 
  recall = true_pos / ( true_pos + false_neg) 
  f1_score = 2 * precision * recall / ( precision + recall ) 
  quality <- list( 'positive' =  positive ,
                  'negative' =  negative ,
                  'pos_prime' =  pos_prime ,
                  'neg_prime' = neg_prime ,
                  'k' =  k ,
                  'error' = error ,
                  'accuracy' = accuracy ,
                  'precision' = precision ,
                  'recall' = recall ,
                  'f1_score' = f1_score
                )  
  print(sprintf("%s: %s", attr(quality, 'names'), quality))
  quality  
}
examples_in_lecture <- function(){
  print("example 1: true_pos= 546, false_pos= 314, false_neg= 0, true_neg= 0")
  quality <- quality.confusion_matrix( true_pos= 546, false_pos= 314, false_neg= 0, true_neg= 0)
  
  print("example 2: true_pos= 544, false_pos= 251, false_neg= 2, true_neg= 63")
  quality <- quality.confusion_matrix( true_pos= 544, false_pos= 251, false_neg= 2, true_neg= 63)
}
examples_in_lecture()
