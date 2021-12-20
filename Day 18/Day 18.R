#Day 18
library(tidyverse)

#Parsenumber  : import a string to a "number" structure.
#Reducenumber : explode and split elements of the number according to rules.
#Addnumbers   : Form a new number by adding two numbers as a pait of numbers.
#Magnitude    : Calculate the magnitude of a number.

#Data structure candidates : 
# - String (keep it as it is)
# - Nested list (each parens is a list level). In R it is hard to mutate this since we cannot mutate a reference to a sublist. Or can we do this with purrr::pluck() ?
# - Flat list (each list element is [,],"," or a number.). Go with this!

input <- readLines("Day 18/test.txt", )    #sample input
input <- readLines("Day 18/input.txt", )   #puzzle input

#Process the input : parse each line and add it to what we have.   ERROR : We process once to many! The next to last number is correct!!
n <- parsenumber(input[1])
for(i in 2:length(input)){
  n2 <- parsenumber(input[i])
  n <- addnumbers(n, n2)
  n <- reducenumber(n)
}
totmag(n)


### Part 2 ####
#What is the largest magnitude of any sum of two different snailfish numbers from the homework assignment?
#Bruteforce : Test all 10000 combinations (both ways), find the largest magnitude. 
maxmag <- 0
for(i in 1:length(input)){
  for(j in 1:length(input)){
    if(!i==j){
    n1 <- parsenumber(input[i])
    n2 <- parsenumber(input[j])
    n <- addnumbers(n1, n2)
    n <- reducenumber(n)
    mag <- totmag(n)
    if(mag > maxmag) {
      maxmag <- mag
      print(c(i,j,mag))}
    }
  }
}
maxmag


##################### Functions ##################################
#recursively process each pair of "real" numbers. 
totmag <- function(n){
  while(length(n)>1){
    n <- magnitude(n)
  }
  return(n)
}

magnitude <- function(n){
  for(i in 1:length(n)){
    #Look for integer comma integer.
    if(is.integer(n[[i]]) && n[[i+1]] == ',' && is.integer(n[[i+2]])){
      leftval  <- n[[i]]
      rightval <- n[[i+2]]
      newval <- as.integer(3 * leftval + 2 * rightval)
      if(i>2) n2 <- c(n[1:(i-2)], newval) else n2 <- c(newval)    #Add anyhting before this pair is there is anything
      if(length(n) > i+3) n2 <- c(n2, n[(i+4):length(n)])         #Add the rest if there is anything after this pair
      return(n2)
    }
  }
}

#add two number : form a new pair.
addnumbers <- function(n1, n2){
  #print('add')
  return(c('[', n1, ',', n2, ']'))
}

#convert a string to a flat list. Assume all numbers are single-digit.
parsenumber <- function(s){
  n <- list()
  for(i in 1:nchar(s)){
    x <-str_sub(s,i,i)
    if(x %in% "0":"9") x <- as.integer(x)
    n <- append(n, x)
  }
  return(n)
}

#reduce a number until no further actions apply.
reducenumber <- function(n){
  n2 <- reduceaction(n)
  if(length(n2) == 0) return(n) else return(reducenumber(n2))
  
}

#reduce a number with a single action. Return an empty list if no action was performed.
reduceaction <- function(n){
  
  # 1: If any pair is nested inside four pairs, the leftmost such pair explodes.
  # Iterate the number, count nest levels, see if we reach a fifth [:
  nestlevel <- 0
  for(i in 1:length(n)){
    if(n[[i]] == '[') nestlevel <- nestlevel + 1
    if(n[[i]] == ']') nestlevel <- nestlevel - 1
    
    #explode any pair nested withing four levels:
    if(nestlevel == 5) {
      #print('explode')

      leftvalue  <- n[[i+1]]
      rightvalue <- n[[i+3]]
      
      #find the next value to the left, add leftvalue to it:
      for(j in i:1){
        if(is.integer(n[[j]])){
          n[[j]] <-  n[[j]] + leftvalue
          break
        }
      }
        
      #find the next value to the right, add righttvalue to it:
      for(j in (i+4):length(n)){
        if(is.integer(n[[j]])){
          n[[j]] <-  n[[j]] + rightvalue
          break
        }
      }
        
        #Then, the entire exploding pair is replaced with the regular number 0.
        n <-c(n[1:(i-1)], list(0L) ,n[(i+5):length(n)] )
        return(n)  #After exploding a pair, do not look any further.
    }
  } #end explode
  
  # 2 : If we did not explode anything : Perform any splits. 
  #browser()
  for(i in 1:length(n)){
    if(is.integer(n[[i]]) && n[[i]] >= 10 ){
      #print('split')
      leftvalue  <- as.integer(floor(  n[[i]] / 2))
      rightvalue <- as.integer(ceiling(n[[i]] / 2))
      
      #replace the large number with the split number.
      n <-c(n[1:(i-1)], list('[' , leftvalue, ',',rightvalue, ']'), n[(i+1):length(n)] )
      return(n)  #After splitting a pair, do not look any further.
    }
  }
  
  return(list()) #If we did not split or explode, return an empty list.
}


