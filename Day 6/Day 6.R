#Day 6
#How many lanternfish would there be after 80 days?
library(stringr)
library(tidyverse)

input <- readLines("Day 6/test.txt", )              #sample input
input <- readLines("Day 6/input.txt", )             #puzzle input
input <-as.integer(unlist(str_split(input, ',')))   #Convert to a vector of integers, one per fish.

#At each day: Count down all values. How many lanternfish would there be after 80 days?
f <- input
for(i in 1:80){
  f <- f-1                                    #Decrease all counters with one
  f <- c(f, rep(8,length(f[f<0]) ))           #Add new fish
  f[f<0] <- 6                                 #Reset negative fish to 6.
}

length(f)   #355386 after 80 days - correct!

########################################################################################################################
#Part 2 : How may laternfish after 256 days? Too many to compute as above...
#Count total nr of fish in a given status (-1 to 8). In each iteration: move total amount down one bin. Total amount in -1 are spawned to 8, added to 6 and deleted from -1.

#First group input by status:
s <- vector(mode="integer",length=10)   
names(s) <- -1:8
for(i in 1:length(input)){
  s[input[i]+2] <- s[input[i]+2] + 1
}

#loop though days:
for(i in 1:256){
  
  #age all fish one step
  for(j in 1:9){
    s[j] <- s[j+1]
  }
  
  #spawn all new fish
  s['8']  <- s['-1']               #Add newborn fish as status 8
  s['6']  <- s['6'] + s['-1']      #Move new parents (in -1) to bin 6
  s['-1'] <- 0                     #Reset -1 bin
}

format(sum(s),scientific = FALSE )  #1613415325809 correct! 


