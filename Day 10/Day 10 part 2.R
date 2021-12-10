#Day 10 - part 2
##################################### Prepare data ##########################################
library(collections)
library(tidyverse)
input <- readLines("Day 10/test.txt", )              #sample input
input <- readLines("Day 10/input.txt", )             #puzzle input

#Look for opening and closing parens in order. Push opening parens on a stack, comapre closing parens with the last opening.
#If the same, remove from stack if not, this is an error.

openers <- c('(', '[', '{', '<')
closers <- c(')', ']', '}', '>')
errors <- vector(mode='character')

##################################### Functions ##########################################
#return remaining closing stuff
completeline <- function(input){
  s <- stack()
  for(j in 1:nchar(input)){
    x <- substr(input,j,j)
    if(x %in% openers){ s$push(x) }
    if(x %in% closers){ 
      o <- s$pop() 
      if(!which(openers == o) == which(closers == x)) return('error')
    }
  }
  #return the remaining stack
  return(unlist(s$as_list()))
}

calcscore <- function(x){
  score <- 0
  for(i in 1:length(x)){
    score <- score * 5
    score <- score + switch(x[i], "(" = 1, "[" = 2, "{" = 3,"<" = 4 )
  }
  return(score)
}

#################################### Execute ##############################################
#calculate all scores, get the middle score : #2360030859 : Correct!
x <- sapply(input, completeline)
x <- subset(x, x != 'error')
x |> sapply(calcscore) |> median()
