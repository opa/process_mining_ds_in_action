# Quality measures


quality.confusion_matrix <- function( true_pos, false_pos, false_neg, true_neg ) {
  positive <- c('positive', true_pos + false_neg)
  negative <- c('negative', true_neg + false_pos)
  pos_prime <- c('pos_prime', true_pos + false_pos)
  neg_prime <- c('neg_prime', true_neg + false_neg)
  k <- c('k', false_neg + true_neg + false_pos + true_neg)
  error <- c('error', (false_pos + false_neg) / as.numeric(k[2]) )
  accuracy <- c('accuracy',  (true_pos + true_neg) / as.numeric(k[2]) )
  precision <- c('precision',  true_pos / ( true_pos + false_pos ) )
  recall <- c('recall', true_pos / ( true_pos + false_neg) )
  f1_score <- c('f1_score' , 2 * as.numeric(precision[2]) * as.numeric(recall[2]) / ( as.numeric(precision[2]) + as.numeric(recall[2]) ) )
  quality <- list(as.numeric( positive[2]) ,
                  as.numeric( negative[2]) ,
                  as.numeric( pos_prime[2]) ,
                  as.numeric( neg_prime[2]) ,
                  as.numeric( k[2]) ,
                  as.numeric( error[2]) ,
                  as.numeric( accuracy[2]) ,
                  as.numeric( precision[2]) ,
                  as.numeric( recall[2]) ,
                  as.numeric( f1_score[2])
                )  
  names(quality) <- c(positive[1] ,
                      negative[1] ,
                      pos_prime[1] ,
                      neg_prime[1] ,
                      k[1] ,
                      error[1] ,
                      accuracy[1] ,
                      precision[1] ,
                      recall[1] ,
                      f1_score[1]
                    )  
   quality  
}
examples_in_lecture <- function(){
  print("example 1: true_pos= 546, false_pos= 314, false_neg= 0, true_neg= 0")
  quality <- quality.confusion_matrix( true_pos= 546, false_pos= 314, false_neg= 0, true_neg= 0)
  print(sprintf("the precision is: %s" , quality$precision))
  print(sprintf("the recall is: %s" , quality$recall))
  print(sprintf("the F1-score is: %s" , quality$f1_score))
  
  print("example 2: true_pos= 544, false_pos= 251, false_neg= 2, true_neg= 63")
  quality <- quality.confusion_matrix( true_pos= 544, false_pos= 251, false_neg= 2, true_neg= 63)
  print(sprintf("the precision is: %s" , quality$precision))
  print(sprintf("the recall is: %s" , quality$recall))
  print(sprintf("the F1-score is: %s" , quality$f1_score))
}
examples_in_lecture()
