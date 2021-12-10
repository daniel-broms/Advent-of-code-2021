#Day 9
library(tidyverse)

#Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?
input <- readLines("Day 9/input.txt")
input <- readLines("Day 9/test.txt")
cols <- nchar(input[1])
input <- as.integer(unlist(strsplit(input, split='')))
t <- matrix(input, ncol = cols, byrow=T)

#Check each point, if it is low add it to a vector.
v <- vector(mode="integer")
lp <- vector(mode="list")
for(i in 1:nrow(t)){
  for(j in 1:ncol(t)){
    n <- getneighbors(t,i,j)
    if(sum(n > t[i,j]) == length(n) ) {
      v <- c(v,t[i,j])
      lp[[length(lp)+1]] <- c(i,j)
    }
  }
}

v
sum(v+1)   #558 : correct.

getneighbors <- function(t,i,j){
  n <- vector(mode="integer")
  if(i>1        ) {n <- c(n, t[i-1,j  ])}
  if(j>1        ) {n <- c(n, t[i,j-1])}
  if(j < ncol(t)) {n <- c(n, t[i,j+1])}
  if(i < nrow(t)) {n <- c(n, t[i+1,j  ])}
  n
}

####################################################################################################
#Part 2: What do you get if you multiply together the sizes of the three largest basins?

#Locations of height 9 do not count as being in any basin and all other locations will always be part of exactly one basin.
#A basin is surrounded by nines.

#Find the sizes of all 240 basins found in part 1. 
#for each basin: Add neighbors to all basin points if the point is not already in the basin and if the neighbors value is not 9.
#Continue growing the basin until no more points are added.

basin_size <- vector(mode="integer")

for(basin in 1:length(lp)){

  #make a data frame for each basin, initialize with the low point coordinates.
  df <- data.frame(Y=lp[[basin]][1], X=lp[[basin]][2])
   
  #loop through all basin members, add new members until no new members are found in a whole iteration
  repeat{
    before <- nrow(df)
      for(i in 1:before){
        df <- addneighbors(t, df[i,1], df[i,2] , df)  #add (new) neighbors to each member
      }
    if(nrow(df)==before){break}
  }
  print(c(basin, nrow(df)))
  basin_size <- c(basin_size ,nrow(df))
}
sort(basin_size, decreasing = T)    #101*94*93 = 882942 : Correct!

#add coordinates of neighbors < 9 which are not already in basin.
addneighbors <- function(t,i,j,df){
  if(i>1         && t[i-1,j]<9 && nrow(filter(df, Y==i-1 & X==j)) ==0  ) {df[nrow(df) + 1,] <-  c(i-1,j)}
  if(j>1         && t[i,j-1]<9 && nrow(filter(df, Y==i & X==j-1)) ==0  ) {df[nrow(df) + 1,] <-  c(i,j-1)}
  if(j < ncol(t) && t[i,j+1]<9 && nrow(filter(df, Y==i & X==j+1)) ==0  ) {df[nrow(df) + 1,] <-  c(i,j+1)}
  if(i < nrow(t) && t[i+1,j]<9 && nrow(filter(df, Y==i+1 & X==j)) ==0  ) {df[nrow(df) + 1,] <-  c(i+1,j)}
  df
}
