###########################################################################################
# Day 1 
###########################################################################################

#Import the input data to an integer vector.
input <- scan(file="Day 1/input.txt", what=integer())   #puzzle input
input <- scan(file="Day 1/test.txt",  what=integer())   #sample input

#Count the number of times a depth measurement increases
#iterate through the list, add one if we increase
inc <- 0
for(i in 1:(length(input)-1)){
  if(input[i+1] > input[i]) {inc <-inc + 1}
}
inc  #1390 : Correct.



###########################################################################################
#Part 2 : 
#Compare sliding 3-measurement sums instead.
inc <- 0
for(i in 1:(length(input)-3)){
  if(input[i+1] + input[i+2] + input[i+3] > input[i] + input[i+1] + input[i+2]) {inc <-inc + 1}
}
inc  #1457 : Correct.




###########################################################################################
#Can we do this in a more elegant functional way?

#base diff() function calculates lagged differences:
sum(diff(input)        > 0)                                       #1390 : correct!
sum(diff(input, lag=3) > 0)                                       #1457 : correct! (note - we do not need to include overlapping values) 


#home-rolled vectorized differencing:
sum((input[2:length(input)] - input[1:(length(input)-1)]) > 0)    #1390 : correct!
sum((input[4:length(input)] - input[1:(length(input)-3)]) > 0)    #1457 : correct!

