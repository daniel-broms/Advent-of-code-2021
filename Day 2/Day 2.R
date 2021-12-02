#Day 2
#Import the input data to an integer vector.
library(stringr)
library(tidyverse)

input <- readLines("Day 2/test.txt", )    #sample input
input <- readLines("Day 2/input.txt", )   #puzzle input


#Organize the input in a two-column matrix.
t <- matrix(unlist(str_split(input, ' ')), ncol=2, byrow=T)

#loop though the matrix, find final position
depth <-0
hpos <- 0

for (i in 1:(length(t)/2)){
  if (t[i,1] == 'forward') {hpos <- hpos + as.integer(t[i,2])}
  if (t[i,1] == 'down')    {depth <- depth + as.integer(t[i,2])}
  if (t[i,1] == 'up')      {depth <- depth - as.integer(t[i,2])}
}
  
hpos * depth   #2272262 : correct

#########################################################################
#Part 2
################################################################

#loop though matrix, find final position - now with aim
depth <-0
hpos  <-0
aim   <-0

for (i in 1:(length(t)/2)){
  if (t[i,1] == 'down')    {aim <- aim + as.integer(t[i,2])}
  if (t[i,1] == 'up')      {aim <- aim - as.integer(t[i,2])}
  if (t[i,1] == 'forward') {
    hpos <- hpos + as.integer(t[i,2])
    depth <- depth + aim * as.integer(t[i,2])
    }
  }

hpos * depth   #2134882034 : Correct


#################################################################
#Try using tidyr #separate" to split the string into two columns:
input <- readLines("Day 2/test.txt", )    #sample input
t <- tibble(input)

# syntax : separate(data frame, col=target column in tibble, into=names of new columns, sep= separator expression)
t <-       separate(t,          col=input,                   into=c('dir', 'qty')     , sep= ' '                 )     


