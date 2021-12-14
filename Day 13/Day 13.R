#Day 13 : How many dots are visible after completing just the first fold instruction on your transparent paper? (y=7 in test, x=655 in input)?
input <-read.table("Day 13/input.txt", col.names = c('x', 'y'), sep="," ) 
folds <-read.table("Day 13/folds.txt", col.names = c('dim', 'at'), sep="=" )  

input <-read.table("Day 13/test.txt", col.names = c('x', 'y'), sep="," )
folds <-read.table("Day 13/testfolds.txt", col.names = c('dim', 'at'), sep="=" )  

#Make matrix with ones where coordinates are:
cols <- max(input$x) + 1  
rows <- max(input$y) + 1 
if(rows %% 2 == 0) rows <- rows + 1 #Fix input, must be odd number of rows!
if(cols %% 2 == 0) cols <- cols + 1 #Fix input, must be odd number of cols!

m <- matrix(rep(0, cols*rows), ncol=cols) #NOTE : m(row col) = m (y, x). Also matrix is 1-based.

for(i in 1:nrow(input)){
  m[input[i,2]+1, input[i,1]+1] <- 1
}

##########################################################################
foldmatrixy <- function(m){
  m2 <- m
  m2[] <- 0
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      m2[nrow(m)-i+1, j] <- m[i,j]
    }
  }
  return(m2)
}

foldmatrixx <- function(m){
  m2 <- m
  m2[] <- 0
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      m2[i, ncol(m)-j+1] <- m[i,j]
    }
  }
  return(m2)
}

#Part 1: fold along 655, count dots. 827:Correct!
sum((foldmatrixx(m[,657:1311]) + m[,1:655]) >0)

######## Part 2 : finish folding all of the paper. Parse each instruction.
for(i in 1:nrow(folds)){
  at <- folds[i, 2]
  
  if(folds[i,1] == 'fold along y'){
    m <- foldmatrixy(m[(at+2):nrow(m), ]) + m[1:at,]
  } else {
    m <- foldmatrixx(m[, (at+2):ncol(m)]) + m[, 1:at]
  }
}

#Show letters : EAHKRECP
m4 <- matrix(rep("", 40*6), ncol=40)
m4[m>0] <- "#"
noquote(m4)

         