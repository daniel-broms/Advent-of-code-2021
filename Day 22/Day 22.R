#Day 22  Part 1 : only process cubes withing -50- 50.
library(collections)
library(tidyverse)

input <- readLines("Day 22/test.txt", )    #sample input
input <- readLines("Day 22/input.txt", )    #puzzle input
t <- tibble(input)
t <-separate(t, 1, c('instr', 'cube'), sep = ' ')
t <-separate(t, col=2,  into=c('x', 'y' ,'z'), sep = ',')
t <-separate(t, col='z', into=c('z1','z2'), sep = '\\.\\.')
t <-separate(t, col='y', into=c('y1','y2'), sep = '\\.\\.')
t <-separate(t, col='x', into=c('x1','x2'), sep = '\\.\\.')
t$x1 <- as.integer(str_replace(t$x1, '[xyz]=', '')) +51
t$y1 <- as.integer(str_replace(t$y1, '[xyz]=', '')) +51
t$z1 <- as.integer(str_replace(t$z1, '[xyz]=', '')) +51
t$x2 <- as.integer(t$x2) +51
t$y2 <- as.integer(t$y2) +51
t$z2 <- as.integer(t$z2) +51


#Create a 3D array with dim = 101
m <- array(dim = c(101,101,101))


#Process instructions, add 51 to each coordinat value since indexes are one-based and cannot be negative.
for(i in 1:(nrow(t))){
  if(abs(t[[i,'x1']] - 51) <= 50){
    print(i)
    if(t[i,'instr']== 'on') {
      m[t[[i, 'x1']]:t[[i, 'x2']], t[[i, 'y1']]:t[[i, 'y2']], t[[i, 'z1']]:t[[i, 'z2']]] <- 1 
      }
    else{
      m[t[[i, 'x1']]:t[[i, 'x2']], t[[i, 'y1']]:t[[i, 'y2']], t[[i, 'z1']]:t[[i, 'z2']]] <- 0  
    }
  }
}
sum(m, na.rm = T) #561032 : Correct!

###################### Part 2 ################################
