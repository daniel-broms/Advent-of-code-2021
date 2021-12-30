#Day 24 Build an ALU processor, find the largest valid fourteen-digit model number that contains no 0 digits. What is the largest model number accepted by MONAD?
library(tidyverse)
library(microbenchmark)

#program <- readLines("Day 24/test.txt", )    #sample input

#Notes:
#The program is a repeating pattern of 18 instructions, repeats once for each input digit. 18*14=252 instructions.
#the input digits constrained as below. (Constraints were found in Excel - this was the hard part!)

#w5=w4-1
#w6=w3-4
#w8=w7+8
#w10=w9+4
#w12=w11+3
#w13=w2+1
#w14=w1-2

program <- readLines("Day 24/input.txt" )   #puzzle input
program <- tibble(program)
program <- separate(program, col=1,  into=c('instr', 'op1' ,'op2'), sep = ' ')
program<- as.matrix(program)   #convert to matrix for speed, 50 faster!

         #12345678901234 
input <- '98998519596997' #the largest number which conforms to the constraints : correct

#Part 2 : find the smallest number which conforms to the rules.
         #12345678901234    
input <- '31521119151421'  #OK!

input <-as.integer(unlist(str_split(input,''))) #convert to integer vector for speed.
register <-c(w=0, x=0, y=0, z=0)
input_pos <- 1

execute_program(program, input)


execute_program <- function(program, input){
  for(i in 1:(18*14)){
    
    instr <- program[[i,'instr']]
    op1   <- program[[i,'op1']]
    op2   <- program[[i,'op2']]
    
    if((i-1) %% 18 == 0) print(c(i-1, floor(i/18), register))  #Show the result of each sub-program

    if(instr=='inp') {
      register[op1] <- input[input_pos]
      input_pos <- input_pos+1
      next
    }
    
    if(op2 %in% names(register)) operand <- register[op2] else operand <- as.integer(op2)
    
    if(instr=='add') {register[op1] <- register[op1] + operand; next} 
    if(instr=='mul') {register[op1] <- register[op1] * operand; next} 
    if(instr=='div') {register[op1] <- floor(register[op1] / operand); next} 
    if(instr=='mod') {register[op1] <- register[op1] %% operand; next} 
    if(instr=='eql') {register[op1] <- ifelse(register[op1] == operand, 1, 0);  next}
  }
  print(c(i-1, floor(i/18), register))
  return(register)
}
