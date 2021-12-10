#Day 10
##################################### Prepare data ##########################################
library(collections)
library(tidyverse)
input <- readLines("Day 10/test.txt", )              #sample input
input <- readLines("Day 10/input.txt", )             #puzzle input

#What is the total syntax error score for those errors?
#A corrupted line is one where a chunk closes with the wrong character.

#Look for opening and closing parens in order. Push opening parens on a stack, comapre closing parens with the last opening.
#If the same, remove from stack if not, this is an error.

openers <- c('(', '[', '{', '<')
closers <- c(')', ']', '}', '>')
errors <- vector(mode='character')

##################################### Functions ##########################################
checkerrors <- function(input){
  for(i in 1:length(input)){
    s <- stack()
    for(j in 1:nchar(input[i])){
      x <- substr(input[i],j,j)
      if(x %in% openers){ s$push(x)}
      if(x %in% closers){
        o <- s$pop()
        if(!which(openers == o) == which(closers == x)){
          errors <- c(errors,x)
          break 
        }
      }
    }
  }
  errors
}

#################################### Execute ##############################################
errors <- checkerrors(input)
sum(sapply(errors, \(x) switch(x, ")" = 3, "]" = 57, "}" = 1197,">" = 25137 )))  #321237 : Correct!


