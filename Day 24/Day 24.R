#Day 24 Build an ALU processor, find the largest valid fourteen-digit model number that contains no 0 digits. What is the largest model number accepted by MONAD?
library(tidyverse)
library(microbenchmark)

#program <- readLines("Day 24/test.txt", )    #sample input

#Notes:
#The program is a repeating pattern of 18 instructions, repeats once for each input digit. 18*14=252 instructions.

program <- readLines("Day 24/input.txt" )   #puzzle input
program <- tibble(program)
program <- separate(program, col=1,  into=c('instr', 'op1' ,'op2'), sep = ' ')
program<- as.matrix(program)   #convert to matrix for speed, 50 faster!

input <- '13579246899999'
input <- '11111111111113'
input <-as.integer(unlist(str_split(input,''))) #convert to integer vector for speed.


register <-c(w=0, x=0, y=0, z=0)
input_pos <- 1

execute_program(program, input)


execute_program <- function(program, input){
  for(i in 1:nrow(program)){
    print(c(i-1, unname(register['z'])))

    instr <- program[[i,'instr']]
    op1   <- program[[i,'op1']]
    op2   <- program[[i,'op2']]

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

return(register)
}


