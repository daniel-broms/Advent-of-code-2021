#Day 11
library(collections)
library(tidyverse)
#How many total flashes are there after 100 steps?

t <- read_fwf("Day 11/test.txt", col_positions = fwf_widths(rep(1,10)))
t <- read_fwf("Day 11/input.txt", col_positions = fwf_widths(rep(1,10)))
t <- t[1:10,]
t <-as.matrix(t)

#in each step : Increase all cells by 1. 
flashes <- 0
for(flash in 1:100){
  t <- t + 1
  
  #Flash all cells > 9
  while(length(t[t>9]) > 0){
    for(i in 1:10){
      for(j in 1:10){
        if(t[i,j] > 9){ 
          flashes <- flashes + 1
          t[i,j] <- 0
          t <- flashcell(t,i,j)
        }
      }
    }
  }
}

flashes #1625 : Correct!

#flash a given cell : increase all surrounding cells with 1, unless they are 0.
flashcell <- function(t,i,j){
  x1 <- max(1,j-1)
  y1 <- max(1,i-1)
  x2 <- min(10,j+1)
  y2 <- min(10,i+1)
  
  t[y1:y2, x1:x2] <- sapply(t[y1:y2, x1:x2], \(x) ifelse(x==0, x, x+1))
  return(t)
}



