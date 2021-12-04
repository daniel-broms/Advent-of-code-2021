#Day 4 
library(stringr)
library(tidyverse)

#input <- readLines("Day 4/test.txt", )    #sample input
input <- readLines("Day 4/input.txt", )   #puzzle input

#Find which board wins bingo, which is the last number, 
nums <- input[1]
nums <- as.integer(unlist(strsplit(nums, ',')))

#Read the input to a tibble as fixed-width 2-character columns.
#t <- read_fwf("Day 4/test.txt",col_positions = fwf_widths(rep(3,5)) )                    
t  <- read_fwf("Day 4/input.txt",col_positions = fwf_widths(rep(3,5)) )

#make a list of boards, each board is a matrix.
#Also make a matching list of hits.
nboards <- 100
boards <- vector("list", nboards)
hits   <- vector("list", nboards)

#Convert each board to a 5x5 matrix, store all boards in a list. 
for(i in 0:(nboards-1)){        #for each board
  boards[[i+1]] <- data.matrix(t[(3 + i * 6):(3 + i * 6 + 4),])
  hits[[i+1]]   <- matrix(rep(0,25), nrow=5)
}   

findwinner <-function(){   
  #Now loop though bing0 numbers, mark hits, and see if we have any complete rows.
  for(n in 1:length(nums)){
    number <- nums[n]
    
    #mark all hits
    for(b in 1:nboards){
      for(i in 1:5){
        for(j in 1:5){
          if(boards[[b]][i,j] == number){
            hits[[b]][i,j] <<- 1
          }
        }
      }
    }
    
    #see if we have any winners, if so return the winner details
    for(b in 1:nboards){
      fullrows <- which(rowSums(hits[[b]]) ==5 )
      fullcols  <- which(colSums(hits[[b]]) ==5 )
      
      if(length(fullrows)>0){
        return(c(b,'row', fullrows[1],  number))
      }
      
      if(length(fullcols)>0){
        return(c(b,'col', fullcols[1],  number))
      }
    } 
  }
}

findwinner() 

#Test : board 3 wins at number 24 at row 1.
#Input : board 42 wins at col 3 with nr 60.

#find sum of unmarked numbers on winning board
wb <- 42
sum((hits[[wb]]-1) * -1 * boards[[wb]])  #782 * 60 = 46920: Correct!


