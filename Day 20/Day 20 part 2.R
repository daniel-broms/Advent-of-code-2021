#Day 20  

#Convert an input image to an output image twice. How many pixels are lit in the resulting image??

#Test input
#input <- readLines("Day 20/test.txt")    #test input

input <- readLines("Day 20/input.txt")    #sample input
input <- str_replace_all(input, '#','1')
input <- str_replace_all(input, '\\.','0')
algo <- input[1]

iterations <- 2

#convert the input image to a matrix
input <- input[3:length(input)]
ncol <- nchar(input[1])
m <- matrix(as.integer(str_split_fixed(input, '', ncol)), ncol =ncol)

#Pad the matrix with extra positions to allow it to grow
padding <- iterations + 2
ncol2 <- padding+ncol+padding
m2 <- matrix(rep(0, ncol2*ncol2), ncol = ncol2)

for(i in 1:ncol){
  for(j in 1:ncol){
    m2[i+padding, j+padding] <- m[i,j]
  }
}

#scan 50 times.
border <- 0  #borders begin with zeroes
for(i in 1:iterations){
  #Scan the image, create a new image. Do not scan outer border.
  m3 <- matrix(rep(0, ncol2*ncol2), ncol = ncol2)
  for(i in 2:(dim(m2)[1]-1)){
    for(j in 2:(dim(m2)[1]-1)){
      
      #get the 9-bit number for position i,j:
      number <- c(m2[(i-1), (j-1):(j+1)], m2[(i), (j-1):(j+1)], m2[(i+1), (j-1):(j+1)])
      number <- str_c(number, collapse='')
      number <- base::strtoi(number, base = 2)
      
      #get the mapped bit from the algo string
      bit <- as.integer(str_sub(algo,number+1, number+1))
      m3[i,j] <- bit
    } 
  }
  
  #If first bit in algo string=1, flip all zeroes in border to 1 also.
  #if the last bit in algo string=0, flip all ones in the border to zero.
  if(str_sub(algo,1,1) == "1" & str_sub(algo,512,512) == "0"){
    border <- ifelse(border==0,1,0)
    m3[1,] <- border
    m3[,1] <- border
    m3[dim(m2)[1], ] <- border
    m3[, dim(m2)[1]] <- border
  }
  m2 <- m3
}

sum(m2)  # Part 1 with 2 iterations: 4873 correct!


#Part 2 ####################################################################################
#Start again with the original input image and apply the image enhancement algorithm 50 times.
#16394 : Correct!
