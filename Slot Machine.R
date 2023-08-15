wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
prob <- c("DD"=0.03, "7"=0.03, "BBB"=0.06, "BB"=0.1, "B"=0.25, "C"=0.01, "0"=0.52)

get_symbols <- function(){
  sample(wheel, size = 3, replace = TRUE, prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols){
  #identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  #get prize
  if(same){
    payouts <- c("DD"=100, "7"=80, "BBB"=40, "BB"=25, "B"=10, "C"=10, "0"=0)
    prize <- unname(payouts[symbols[1]])
  }
  else if(all(bars)){
    prize <- 5
  }
  else{
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries+1]
  }
  #adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize*2^diamonds
}

play <-function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

slot_display <- function(prize){
  
  #extract symbols
  symbols <- attr(prize, "symbols")
  
  #collapse symbols into single string1
  symbols <- paste(symbols, collapse = " ")
  
  #combine symbol with prize as a regular expression
  string <- paste(symbols, prize, sep = "\n$")
  
  #display regular expression in console without quotes
  cat(string)
}

slot_display(play())

#calculating probability for each possible combo on the wheel
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
combos$prob <- combos$prob1*combos$prob2*combos$prob3

#determining prize for each combo

