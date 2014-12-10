calculate.entropy.node <- function(nums) {
  #takes in vector of number of each incidence of possible outcomes
  result <- 0
  count_possible_outcome <- length(nums)
  # SUM for all cases ( (probility_ncase) log( probility_ncase, base = num_cases) )
      ### need to verify the log base changes based on the possible outcomes e.g. log2 when binary outcome ... log3 when ternary outcome
  log_base <- if (count_possible_outcome == 1) 2 else count_possible_outcome #if only single outcome use two anyway - result is entropy zero  (log2(1)=0)
  total_population <- sum(nums)
  for (i in 1:count_possible_outcome) {
      probability_outcome <- nums[i]/total_population
      # in the case of zero log is infinite but multiplied by outcome of value zero - this is just 0 - so result doesn't change on this round
      if (probability_outcome != 0) { 
        result = result + probability_outcome * log(probability_outcome, base=log_base)  
      }
  }
  result
}
calculate.entropy.tree <- function(list_node_outcomes) {
  entropy_each_leaf <- sapply( list_node_outcomes , calculate.entropy.node )
  total_population_tree <- sum ( sapply( list_node_outcomes , sum ) )
  weigthed_average_multiple_on_tree <- ( sapply( list_node_outcomes , sum ) / total_population_tree )
  sum( weigthed_average_multiple_on_tree * entropy_each_leaf )
}

print_examples_in_lecture <- function() {

  print("pass fail course example -- 160 student, 60 fail/100pass")
  tree.whole_population <- list( c(100,60) )
  tree.smokers <- list( c(25,15) , c(75,45) )
  print("compare all students to results of smokers")
  information_gain <- calculate.entropy.tree( tree.smokers ) - calculate.entropy.tree( tree.whole_population )
  print (sprintf("information gains is %s", information_gain))

  
  print("compare all students to results based upon deliniation of gender")
  tree.gender <- list( c(30,50) , c(70,10) )
  information_gain <- calculate.entropy.tree( tree.gender ) - calculate.entropy.tree( tree.whole_population )
  print(sprintf("information gains is %s", information_gain) )
  
  
  print("compare all students to results based upon students who attended_all_lectures")
  tree.attendance <- list( c(20,60) , c(80,0) )
  information_gain <- calculate.entropy.tree( tree.attendance ) - calculate.entropy.tree( tree.whole_population )
  print(sprintf("information gains is %s", information_gain))
  
  print("just testing to see what would happen if there were three possible outcomes - not covered in lecture - example - fail, pass, honors")
  tree.attendance <- list( c(10,60,10) , c(40,0,40) )
  information_gain <- calculate.entropy.tree( tree.attendance ) - calculate.entropy.tree( tree.whole_population )
  print(sprintf("information gains is %s", information_gain))        
}
print_examples_in_lecture()

