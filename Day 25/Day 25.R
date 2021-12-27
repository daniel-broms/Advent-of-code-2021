#Day 25 : What is the first step on which no sea cucumbers move?

cols <- 139
rows <- 137
t <- read_fwf("Day 25/input.txt",col_positions = fwf_widths(rep(1,cols)) ) 
m <- as.matrix(t, ncol=cols)

 
iteration <- 0
repeat{
  moves <- 0
  iteration <- iteration + 1
  
  #first move eastbound sea cucumbers with nothing blocking them:
  m2 <- m
  for(row in 1:rows){
    for(col in 1:cols){
      if(m[row,col] == '>' & m[row, wrapcol(col+1)] == '.'){
        m2[row, col] <- '.'
        m2[row, wrapcol(col+1)] <- '>'
        moves <- moves + 1
      } 
    }
  }
  m <- m2
  
  #then move southbound sea cucumbers with nothing blocking them, after the eastbound ones have moved:
  m2 <- m
  for(row in 1:rows){
    for(col in 1:cols){
      if(m[row,col] == 'v' & m[wraprow(row+1), col] == '.'){
        m2[row, col] <- '.'
        m2[wraprow(row+1), col] <- 'v'
        moves <- moves + 1
      }
    }
  }
  m <- m2
  
  if(moves == 0) break
}
iteration #530 : correct!


wrapcol <- function(x){
  ifelse(x>cols, x-cols, x)
}

wraprow <- function(x){
  ifelse(x>rows, x-rows, x)
}

print(m, quote=FALSE)
