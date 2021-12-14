#Day 4 
library(stringr)
library(tidyverse)

#input <- readLines("Day 4/test.txt", )    #sample input
input <- readLines("Day 4/input.txt", )   #puzzle input

#The first input line contains the bingo number sequence.
nums <- input[1]
nums <- as.integer(unlist(strsplit(nums, ',')))

#Read the input to a tibble as fixed-width 2-character columns.
#t <- read_fwf("Day 4/test.txt",col_positions = fwf_widths(rep(3,5)) )                    
t  <- read_fwf("Day 4/input.txt",col_positions = fwf_widths(rep(3,5)) )

#make a list of boards, each board is a matrix.
#Also make a matching list of hits.
nboards <- 100
boards  <- vector("list", nboards)
hits    <- vector("list", nboards)
winners <- vector(mode = "integer")

#Convert each board to a 5x5 matrix, store all boards in a list. 
for(i in 0:(nboards-1)){        
  boards[[i+1]] <- data.matrix(t[(3 + i * 6):(3 + i * 6 + 4),])
  hits[[i+1]]   <- matrix(rep(0,25), nrow=5)
}   

#Mark all hits for a drawn number.
drawnumber <-function(number){   
  for(b in 1:nboards){
    hits[[b]][boards[[b]] == number] <<-1
  }
}

#see if we have any new winners, if so add them to the list.
checkwinners <- function(){
    for(b in 1:nboards){
    fullrows  <- which(rowSums(hits[[b]]) == 5 )
    fullcols  <- which(colSums(hits[[b]]) == 5 )
    
    if(length(fullrows)>0 | length(fullcols)>0 ){
      if(! b %in% winners){
        winners <<- c(winners, b)
      }
    }
  } 
}

#Draw numbers until the last board wins.
playbingo <- function(){
  for(n in 1:length(nums)){
    number <- nums[n]
    drawnumber(number)
    checkwinners()
    if(length(winners)==nboards) {
      return(c(n,number, winners[nboards]))
    }
  }
}

playbingo()   #board 30 wins last at number 35 (the 84:th number)

#find sum of unmarked numbers on winning board
wb <- winners[nboards]
sum((hits[[wb]]-1) * -1 * boards[[wb]])     #361 * 35 = 12635 : Correct!

######################################
#Find a better way to convert 
