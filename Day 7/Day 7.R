#Day 7
#Determine the horizontal position that the crabs can align to using the least fuel possible. How much fuel must they spend to align to that position?

input <- readLines("Day 7/test.txt", )              #sample input
input <- readLines("Day 7/input.txt", )             #puzzle input

input <-as.integer(unlist(strsplit(input, ',')))

#test all positions from max to min.
maxpos <- max(input)
minpos <- min(input)

#Calculate the total fuel for each position between max and min.
minfuel <- .Machine$integer.max
for(i in minpos:maxpos){
  fuel <- sum(abs(input-i))  
  if(fuel < minfuel) {
    minfuel <- fuel
    print(c(i, fuel))}
}

#####################################################################################################
#Part 2 : Each step becomes one more expensive.
#instead of abs(pos1-pos2), make a function to calculate the fuel required between two positions: 
calcfuel <- function(p1, p2){
  if(p1==p2) {return(0)}
  inc   <- 0
  fuel  <- 0
  steps <- abs(p1-p2)
  
  for(i in 1:steps){
    inc  <- inc  + 1
    fuel <- fuel + inc
  }
  fuel
}

calcfuelv <- Vectorize(calcfuel)

minfuel <- .Machine$integer.max
for(i in minpos:maxpos){
  fuel <- sum(calcfuelv(input, i)) 
  if(fuel < minfuel) {
    minfuel <- fuel
    print(c(i, fuel))}
}
#98039527 : correct

