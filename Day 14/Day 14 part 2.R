#Day 14
library(tidyverse)
options(scipen=999)                        #Skip scientific notation
input <- readLines("Day 14/test.txt", )    #sample input
input <- readLines("Day 14/input.txt", )   #puzzle input

#Apply insertion rules in 10 steps. Each step is simultaneous
s <- input[1]
pi <-input[3:length(input)] 

#Part 2 : We need a new strategy, the string grows too long. 
#Keep one bin per pair of letters: a named vector where name=letter pair and value  current number of these pairs.

#input string into pairs, add them to the pairs vector with counts.
sp <- c()
for(i in 1:(nchar(s)-1)){
  sp <- c(sp, str_sub(s, i, i+1))
}

pairs <-table(sp)  #Named vector with initial pair count

for(step in 1:10){
  
  newpairs <- vector()
  
  #Apply each rule, populate newpairs.
  for(i in 1:length(pi)){
    p  <- str_sub(pi[i],1,2)
    n1 <- str_c( c( str_sub(pi[i],1,1), str_sub(pi[i],7,7)), collapse="")
    n2 <- str_c( c( str_sub(pi[i],7,7), str_sub(pi[i],2,2)), collapse="")
    if (!is.na(pairs[p])){
      newpairs[n1] <-  ifelse(is.na(newpairs[n1]), 0, newpairs[n1]) + pairs[p]
      newpairs[n2] <-  ifelse(is.na(newpairs[n2]), 0, newpairs[n2]) + pairs[p]
    }
  }
  pairs <- newpairs
}

#Count how many we have of each number in the pairs. 
#Count totals per letter:
elements <-vector()
for(i in 1:length(pairs)){
  l1 <- str_sub(names(pairs[i]),1,1)
  l2 <- str_sub(names(pairs[i]),2,2)
  elements[l1] <- ifelse(is.na(elements[l1]), 0, elements[l1]) + pairs[i]
  elements[l2] <- ifelse(is.na(elements[l2]), 0, elements[l2]) + pairs[i]
}

sort(elements/2) #Halve the number per letter and sort the result, find max-min value. Note that the first and last letter have 1 less in all pairs, this need to be adjusted.
#10 iterations : 3425 - 574 = 2851 : correct!
#40 iterations : 10159740693876 - 156927414539 = 10002813279337:  correct!
