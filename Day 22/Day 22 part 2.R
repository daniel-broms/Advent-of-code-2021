#Day 22  Part 2 : much larger cubes now.
library(collections)
library(tidyverse)

input <- readLines("Day 22/test2.txt", )    #sample input
#input <- readLines("Day 22/input.txt", )    #puzzle input
t <- tibble(input)
t <-separate(t, 1, c('instr', 'cube'), sep = ' ')
t <-separate(t, col=2,  into=c('x', 'y' ,'z'), sep = ',')
t <-separate(t, col='z', into=c('z1','z2'), sep = '\\.\\.')
t <-separate(t, col='y', into=c('y1','y2'), sep = '\\.\\.')
t <-separate(t, col='x', into=c('x1','x2'), sep = '\\.\\.')

t$x1 <- as.integer(str_replace(t$x1, '[xyz]=', ''))
t$y1 <- as.integer(str_replace(t$y1, '[xyz]=', '')) 
t$z1 <- as.integer(str_replace(t$z1, '[xyz]=', '')) 
t$x2 <- as.integer(t$x2) 
t$y2 <- as.integer(t$y2) 
t$z2 <- as.integer(t$z2) 
t$index <- as.integer(rownames(t))

#the cubes now go from over 120000 in each dimension. x1 is always < x2 etc.


#Maybe we should track individual cubes, not each cell. 
#Track overlaps - or create subcubes when a new cube overlaps with an existing cub?
#Note that an instruction will completely replace all other cubes within its confines : these cubes can be removed.


#There are 420 cubes, and need to be processed in order. 
#Each cube is assigned an order index, beginning at one.

#For each new cube:
# Any previous cubes which are completely inside this cube can be deleted.
# Any previous cubes which are completely outside this cube can be ignored.
# Any cubes which overlap this cube need to be split, into max 10 subcubes (worst case) which do not overlap this cube. All these cubes have the same order index as the original cube.
# After all splits, we can again check for any cubes before this cube which is inside this cube,

#split cube c1 (the lower levelcube) into cubes which do not overlap c2 (the upper level cube which covers c1)
splitcube <- function(c1, c2)

#check two cubes  - if there is no overlap (0), if c2 is completely inside c1 (1), if c1 is completely inside c2 (2), or partial overlap (3)
checkcubeoverlap <- function(c1, c2){
  
  
}

#check two cube dimensions - if there is no overlap (0), if c2 is completely inside c1 (-1), if c1 is completely inside c2 (1), or partial overlap (3)
checkdimoverlap <- function(c1d1, c1d2, c2d1, c2d2){
  if( between(c1d1, c2d1, c2d2) &  between(c1d2, c2d1, c2d2)){return(1)}
  if( between(c2d1, c1d1, c1d2) &  between(c2d2, c1d1, c1d2)){return(-1)}
  if(!between(c1d1, c2d1, c2d2) & !between(c1d2, c2d1, c2d2)){return(0)}
  return(3)
}

checkdimoverlap(5,15,10,20)



