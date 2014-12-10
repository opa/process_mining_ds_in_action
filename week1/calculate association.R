# where x is the source property
# and y is the outcome  property
# Nx is count of all x in group
# Ny is count of all y in group
# Nxy is count of the intersection of x and y in group
# Nall is the count of complete study group including those without x or y


calc.support <- function( n_combo , n_whole_population , threshold = 0.1 ){
  ## Support (values between 0=bad and 1=good)
  ## support(X => Y) = Number of instances inside ( union of X and Y ) divided by ( / ) the Number of instances occurred in whole pop
  
  # =Nxy / Nall
  support <- n_combo / n_whole_population
  has_support <- identical(TRUE, support > threshold )
  c(support, has_support)
}

calc.confidence <- function( n_combo, n_combo_either_or, threshold = 0.80 ) {
  ## confidence (values between 0=bad and 1=good)
  ##   it is the count of XY intersection ( e.g. count of where X and Y true studying X combination that produced Y outcome) divided buy ( / ) number of X combination  
  # = Nxy / Nx
  confidence <- n_combo / n_combo_either_or
  has_confidence <- identical(TRUE, confidence > threshold ) 
  c(confidence, has_confidence)
}
                                                               
  
  
calc.lift <- function( n_combo, n_pop, n_one_of, n_other_of, threshold = 0.05) {
  ## lift( X=> Y ) = Nxy * Nall / ( Nx * Ny )
  
  ##   it is an indication that X is correlated with Y 
  ##  e.g. calculation ( count of {combo} * count {all} in population ) / ( N{pampers} in pop * N{dommelsch} in pop )
  ##  if:
  ##       lift(X=>Y) > 1 ---- if X and Y are positively correlated
  ##       lift(X=>Y) near 1 --- if X and Y are independent
  ##       lift(X=>Y) < 1 --- if X and Y are negatively correlated
  
  lift_val <- n_combo * n_pop / ( n_one_of * n_other_of )
  if (lift_val > ( 1 + threshold ) )  {
    c(lift_val, "positive correlation") 
  } else if (lift_val < ( 1 - threshold ) ) {
    c(lift_val, "negative correlation") 
  } else {
    c(lift_val, "independent / not correlated")
  }
}

calculate.association <- function( num_x, num_y, num_xy, num_all, support_threshold = 0.1, confidence_threshold = 0.8, lift_threshold = 0.05){
  support_case <- calc.support( num_xy, num_all, support_threshold)
  confidence_case <- calc.confidence ( num_xy , num_x, confidence_threshold )  
  lift_case <- calc.lift( num_xy , num_all , num_x,  num_y, lift_threshold )  
  keys = c("support", "confidence", "lift")
  association <- list(support_case, confidence_case, lift_case)
  names(association) <- keys
  association
}

test_not <- function(val){
  # just to print a pretty sentence
  if (val == TRUE) {
    ""
  } else {
    "not"
  }    
}

show_results <- function(num_x, num_y, num_xy, num_all){
  association <- calculate.association( num_x, num_y, num_xy, num_all, 0.1, 0.8, 0.05 )
  #print 3 lines of association values per test
  print (sprintf("   %s supported by value of %s", 
                 test_not(association$support[2]), 
                 association$support[1]
                 )
         )
  print (sprintf("   %s confident by value of %s", 
                 test_not(association$confidence[2]), 
                 association$confidence[1]
                 )
         )
  print (sprintf("   %s with lift value of %s", 
                 association$lift[2], 
                 association$lift[1]
                 )
         )
}

diaper_example_analysis <- function() {
  print("100 customers buy diapers and/or beer:")
  print("   9 customers buy just Hoegaarden")
  print("  40 customers buy just Pampers")
  print("  50 customer buy just Pampers and Dommelsch")
  print("   1 customer buys Pampers, Hoegaarden and Dommelsch")
  # whole sample size is 100
  num_all <- 100
  print("")
  print("{Pampers} => {Dommelsch}")
    #support= {pampers,dommelsch}/{all_purchases}
    #confidence= {pampers,dommelsch}/{all_pampers_purchases}
    #lift= {pampers,dommelsch}{all_purchases} / ( {all_pamper_purchases} * {all dommelsch purchases} )  
  show_results( num_x = 91, 
                num_y = 51, 
                num_xy = 51, 
                num_all = 100
               )    
  print("{Pampers} => {hoegaarden}")  
  #support= {pampers,hoegaarden}/{all_purchases} 
  #confidence= {pampers,hoegaarden}/{all_pampers_purchases} 
  #lift= {pampers,hoegaarden}{all_purchases} / ( {all_pamper_purchases} * {all hoegarden purchases} )"   
  show_results( num_x = 91, 
                num_y = 10, 
                num_xy = 1, 
                num_all = 100
               )
  print("{Dommelsch} => {Pampers}")  
  #support= {pampers,dommelsch}/{all_purchases} 
  #confidence= {pampers,dommelsch}/{all_dommelsch_purchases} 
  #lift= {pampers,dommelsch}{all_purchases} / ( {all_pamper_purchases} * {all dommelsch purchases} )    
  show_results( num_x = 51, 
                num_y = 91, 
                num_xy = 51, 
                num_all = 100
              )  
  print("{pampers, dommelsch} => {hoegaarden}")  
  #support= {pampers,dommelsch,hoegaarden}/{all_purchases} 
  #confidence= {pampers,dommelsch, hoegaarden}/{all_pampers_dommelsch_purchases}  
  #lift= {pampers,dommelsch,hoegaarden}{all_purchases} / ( {all_pampers_dommelsch_purchases} * {all Hoegaarden purchases} ) 
  show_results( num_x = 51, 
                num_y = 10, 
                num_xy = 1, 
                num_all = 100  
               )
}
diaper_example_analysis()


