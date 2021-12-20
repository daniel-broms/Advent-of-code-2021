#Day 15
library(tidyverse)

#What is the lowest total risk of any path from the top left to the bottom right?
#Can it be formulated as an optimization problem (min sum)? What are the constraints?
#Find distance of ALL nodes from initial node in a parallell matrix (10*10),
#begin at initial node. Find the distnace of each neghbor as (0 + neigbor value).
#Then find distances to all neighbors reachable from visited neighbors.
#Mark nodes where we have checked all neigbors as visited, do not visit it again.

#Read distance matrix
t <- read_fwf("Day 15/test.txt",col_positions = fwf_widths(rep(1,10)) )
t <- read_fwf("Day 15/input.txt",col_positions = fwf_widths(rep(1,100)) ) 
t <- as.matrix(t)

#Expand the matrix five times in each direction:
#First five times right
ti <- t
for(i in 1:4){
  ti <- ti + 1
  ti[ti==10] <- 1
  t <- cbind(t, ti)
}

#Then five times down
ti <- t
for(i in 1:4){
  ti <- ti + 1
  ti[ti==10] <- 1
  t <- rbind(t, ti)
}
t<- as.matrix(t)

rows <- nrow(t)

#Total distance matrix
totd <- t
totd[] <- Inf
totd[1,1] <- 0

#Visited matrix. Set the first cell as visited.
visited <- matrix(rep(F,rows*rows), nrow=rows)
visited[1,1] <- T

#Initialize the first distances from top left (these are special cases since the top left value is not counted).
totd[1,2] <- t[1,2]
totd[2,1] <- t[2,1]

candidates <- c(which(visited == F & totd < Inf)) #Candidate list, in single-number matrix index fomr.


#Visit cells which have not been visited. Pick the unvisited cell with the least total distance value.
#Too slow for part 2 : We need to keep track of candidates instead of searching the whole matrix for the next candidates.
#Add candidates to candidate list when their distance is set, remove them when they are visited.
repeat{
  
  #find the next cell to visit.
  mindistance <- min(totd[candidates])
  i <- candidates[totd[candidates] == mindistance][1]

  row <- i %% rows 
  if(row==0) row<-rows
  col <- ceiling(i/rows)
  
  visit(row, col)
  
  if(sum(visited == F) == 0) break
  
}

totd[rows,rows]
#748  : Correct!



#Visit a cell : Calculate tentative distances to all neigbors, mark it as visited.
visit <- function(row,col){
  if(row>0 & row<=rows & col > 0 & col <=rows){
    if(!visited[row,col]){
      update_totd(row, col, row, col+1)
      update_totd(row, col, row+1, col)
      update_totd(row, col, row-1, col)
      update_totd(row, col, row, col-1)
      visited[row,col]  <<- T
      candidates <<- candidates[candidates != ((col-1)*rows + row)]   #Remove visited cell from candidates.
    }
  }
  
}

#Update totd for r2,c2 if its current distance > total distance from r1,r2:
update_totd <- function(r1, c1, r2, c2){
  if(r2>0 & r2<=rows & c2 >0 & c2 <=rows){
    if(!visited[r2,c2]){
      if(totd[r2, c2] > totd[r1, c1] + t[r2, c2]) { 
        totd[r2, c2] <<- totd[r1, c1] + t[r2, c2]
        candidates <<- c(candidates, (c2-1)*rows + r2)}   #Register this as a new candidate
    }
  }
}
