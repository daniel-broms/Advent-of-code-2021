#Day 14
library(tidyverse)
input <- readLines("Day 14/test.txt", )    #sample input
input <- readLines("Day 14/input.txt", )   #puzzle input

#Apply insertion rules in 10 steps. Each step is simultaneous
s <- input[1]
pi <-input[3:length(input)] 

#Strategy Part 1 : Split the string into overlapping pairs.
#Apply rules to all pairs.
#Then join together pairs to a new string : Remove the first letter from all pairs except the first, then join them.
#Reapaet 10 times.

for(step in 1:10){
  #Split current string into pairs
  sp <- c()
  for(i in 1:(nchar(s)-1)){
      sp <- c(sp, str_sub(s, i, i+1))
  }
  sp
  
  #Apply rules to all pairs
  for(i in 1:length(pi)){
    for(j in 1:length(sp)){
      if(sp[j]==str_sub(pi[i],1,2)){
        sp[j] <- str_c( str_sub(sp[j],1,1) , str_sub(pi[i],7,7), str_sub(sp[j],2,2))
      }
    }
  }
  
  #join together everything again
  s <- str_c(  c(str_sub(s,1,1), str_sub(sp, 2)), collapse = "")
}
s
nchar(s)
#find the most common element and the least common element

s2 <- str_split_fixed(s, '', n=Inf)
max(table(s2)) - min(table(s2))      #2851: Correct!


