#Day 8
input <- readLines("Day 8/test.txt", )              #sample input
input <- readLines("Day 8/input.txt", )             #puzzle input

#part 1 : In the output values, how many times do digits 1, 4, 7, or 8 appear?
#in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

input
input <- unlist(strsplit(str_sub(input, 62,1000), split=' '))

s<-0
for(i in 1:length(input)){
    l <- nchar(input[i])
    if(l==2 | l==4 | l==3 | l==7){s <- s + 1}
}
s

#####################################################################################################
#Part 2
#For each entry, determine all of the wire/segment connections and decode the four-digit output values. 
#What do you get if you add up all of the output values?
#We can use the segment overlap between characters as clues.
input <- readLines("Day 8/test.txt", )              #sample input
input <- readLines("Day 8/input.txt", )             #puzzle input

input_test   <- str_sub(input, 1 ,59)
input_output <- str_sub(input, 62,max(nchar(input)))  

vdecoderow(input_test, input_output) |> sum()         #1019355: correct!

decoderow <- function(test,output){
  
  test <-unlist(strsplit(test, split = ' '))
  
  #1,7,4,8 are simple since their segment lengths are unique.
  one   <- test[nchar(test)==2]
  test  <- test[!test %in% one]
  
  seven <- test[nchar(test)==3]
  test  <- test[!test %in% seven]
  
  four  <- test[nchar(test)==4]
  test  <- test[!test %in% four]
  
  eight <- test[nchar(test)==7]
  test  <- test[!test %in% eight]
  
  #six is the combination with 6 segments with 2 segments in common with 7.
  six   <- test[nchar(test)==6 & visect(seven, test)==2]
  test  <- test[!test %in% six]
  
  #nine is the combination with 6 segments with 4 segments in common with 4.
  nine  <-  test[nchar(test)==6 & visect(four, test)==4]
  test  <- test[!test %in% nine]
  
  #Zero is the remaining 6-segment value.
  zero  <- test[nchar(test)==6]
  test  <- test[!test %in% zero]
  
  #two is the five-segment value with 2 segments in common with 4.
  two   <- test[nchar(test)==5 & visect(four, test)==2]
  test  <- test[!test %in% two]
  
  #five is thee five-segment value with 1 segments in common with 1
  five  <- test[nchar(test)==5 & visect(one, test)==1]
  test  <- test[!test %in% five]
  
  #three is the only remaining value now.
  three <- test
  
  #Save the codes in an ordered, named vector. 
  codes <- c(zero, one, two, three, four, five,six, seven, eight, nine)
  names(codes) <- c(0:9)

  #Now decode the four "output" values to a four-digit integer and return this.
  output               |>
    strsplit(' ')      |>
    unlist()           |>
    vmatchcode(codes)  |>
    paste(collapse="") |>
    as.integer()
}
vdecoderow <- Vectorize(decoderow)


#find how many characters are in common for two strings.
isect <- function(vec, val){
  length(intersect(unlist(strsplit(val, split='')), 
                   unlist(strsplit(vec, split=''))))
}
visect <- Vectorize(isect)

#find the number which matches a code.
matchcode <- function(val, codes){
  for(i in 1:length(codes)){
    if(nchar(val) == nchar(codes[i]) && isect(val, codes[i]) == nchar(val)){
      return(names(codes[i]))
    }
  }
}
vmatchcode <- Vectorize(matchcode, vectorize.args = "val")  #vectorize over "val" (but not over "codes")




