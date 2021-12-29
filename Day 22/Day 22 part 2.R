#Day 22  Part 2 : much larger cubes now.
library(collections)
library(tidyverse)
options(scipen=999)

#input <- readLines("Day 22/test2.txt", )     #sample input. test0.txt works (gives 39 in total volume) test.txt works (=590784). test2.txt works(2758514936282235)

input <- readLines("Day 22/input.txt", )    #puzzle input
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
t$onoff <- as.integer(t$instr == 'on')

#the cubes now go from over 120000 in each dimension. x1 is always < x2 etc.
#Track individual cubes, not each cell. 
#Create subcubes when a new cube overlaps with an existing cube.

#There are 420 cubes, and need to be processed in order. 
#Each cube is assigned an order index, beginning at one.

#For each new cube:
# Any previous cubes which are completely inside this cube can be deleted.
# Any previous cubes which are completely outside this cube can be ignored.
# Any cubes which overlap this cube need to be split, into max 6 subcubes (worst case) which do not overlap this cube. All these cubes have the same order index as the original cube.
# After all splits, we can again check for any cubes before this cube which is inside this cube,

################ Execute ####################
x <- processcubes()
totalvolume(x)       #1322825263376414: correct!

################ Functions ####################

#calculate the volume of a single cube.
cubevolume <- function(A){
  unname((A['x2'] - A['x1'] + 1) * (A['y2'] - A['y1'] + 1) * (A['z2'] - A['z1'] + 1))
}

#calculate the total volume of all cubes.
totalvolume <- function(x){
  totvol <- 0
  for(i in 1:length(x)) totvol <- totvol + cubevolume(x[[i]])
  return(totvol)
}

#process instruction list, return a list of non-overlapping cubes after each step.
processcubes <- function(){
  
  cubes <- list()
  for(i in 1:nrow(t)){
    newcubes <- list()
    A <- unlist(t[i,c(2:9)])
    for(j in seq_len(length(cubes))){
      newcubes <- c(newcubes, splitcube(A,cubes[[j]]))
    }
    
    #Add the new cube last in the list (if it is "on") and replace the master list with the new list.
    if(A['onoff'] == 1) newcubes[[length(newcubes)+1]] <- A
    cubes <- newcubes
    print(c(i, length(cubes), totalvolume(cubes)))
  }
  return(cubes)
}

#split cube B (the lower levelcube) into cubes which do not overlap A (the upper level cube which covers B), a maximum of 6 cubes.
#If no overlap : return the original B cube.
#If total overlap : return nothing (cube B is gone).
#Input format:  A=c(x1,x2,y2,y2,z1,z2, index, onoff)
#Output : A list of B cubes, between 0 and 6 st.
splitcube <- function(A, B){
  
  #if no overlap (i.e. any dimesion doe not overlap), return B unchanged.
  if(   !(between(B['x1'], A['x1'], A['x2']) | between(B['x2'], A['x1'], A['x2']) | between(A['x1'], B['x1'], B['x2']) & between(A['x2'], B['x1'], B['x2']))
      | !(between(B['y1'], A['y1'], A['y2']) | between(B['y2'], A['y1'], A['y2']) | between(A['y1'], B['y1'], B['y2']) & between(A['y2'], B['y1'], B['y2']))
      | !(between(B['z1'], A['z1'], A['z2']) | between(B['z2'], A['z1'], A['z2']) | between(A['z1'], B['z1'], B['z2']) & between(A['z2'], B['z1'], B['z2']))
    ){
    return(list(B))
  }
  
  #if complete overlap (i.e. B is inside A in all dimensions), return an empty list.
  if( between(B['x1'], A['x1'], A['x2']) & between(B['x2'], A['x1'], A['x2']) &
      between(B['y1'], A['y1'], A['y2']) & between(B['y2'], A['y1'], A['y2']) &
      between(B['z1'], A['z1'], A['z2']) & between(B['z2'], A['z1'], A['z2'])){
    return(list())
  }
  
  #We have overlap in all dimensions: Make a list of subcubes where B is split in dimensions x, y and z.
  L <- list()
  #1: Cut in X dimension : one cube to the left of A another to the right of A.
  if(B['x1'] <=  A['x1'] & B['x2'] >=  A['x1']){
    B2 <- B
    B2['x2'] <- A['x1'] - 1
    L[[length(L)+1]] <- B2
  }
  if(B['x2'] >=  A['x2'] & B['x1'] <=  A['x2']){
    B2 <- B
    B2['x1'] <- A['x2'] + 1
    L[[length(L)+1]] <- B2
  }
  
  #2: Cut in y dimension : one cube under A another above A. X dims are bounded by A.
  if(B['y1'] <=  A['y1'] & B['y2'] >= A['y1']){
    B2 <- B
    B2['y2'] <- A['y1'] - 1
    B2['x1'] <- max(A['x1'], B2['x1']) ; B2['x2'] <- min(A['x2'], B['x2'])
    L[[length(L)+1]] <- B2
  }
  
  if(B['y2'] >=  A['y2'] & B['y1'] <= A['y2']){
    B2 <- B
    B2['y1'] <- A['y2'] + 1
    B2['x1'] <- max(A['x1'], B2['x1']) ; B2['x2'] <- min(A['x2'], B['x2'])
    L[[length(L)+1]] <- B2
  }
  
  #3: Cut in z dimension : one cube under A another above A. X and Y dims are bounded by A.
  if(B['z1'] <=  A['z1'] & B['z2'] >= A['z1']){
    B2 <- B
    B2['z2'] <- A['z1'] - 1
    B2['x1'] <- max(A['x1'], B2['x1']) ; B2['x2'] <- min(A['x2'], B['x2'])
    B2['y1'] <- max(A['y1'], B2['y1']) ; B2['y2'] <- min(A['y2'], B['y2'])
    L[[length(L)+1]] <- B2
  }
  
  if(B['z2'] >=  A['z2'] & B['z1'] <= A['z2']){
    B2 <- B
    B2['z1'] <- A['z2'] + 1
    B2['x1'] <- max(A['x1'], B2['x1']) ; B2['x2'] <- min(A['x2'], B['x2'])
    B2['y1'] <- max(A['y1'], B2['y1']) ; B2['y2'] <- min(A['y2'], B['y2'])
    L[[length(L)+1]] <- B2
  }
  return(L)
}



