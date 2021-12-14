#Day 3
library(stringr)
library(tidyverse)

#t <- read_fwf("Day 3/test.txt" ,col_positions = fwf_widths(rep(1,5 )) )

t <- read_fwf("Day 3/input.txt",col_positions = fwf_widths(rep(1,12)) )                #Read input as fixed-length text columns into a tibble. Each column has width 1.       
s <- colSums(t)                                                                        #Find the sum of each column

paste(as.integer(s*2>=nrow(t)), collapse = "")  %>% strtoi(base=2)   #844              #Compare each sum to total nr of rows. Convert boolean to integer, combine to a binary nr and convert to decimal.
paste(as.integer(s*2< nrow(t)), collapse = "")  %>% strtoi(base=2)   #3251

#844 * 3251 = 2743844  Correct!

#Alternative formulation with pipes:
(s*2>=nrow(t))         |>
  as.integer()         |>
  paste(collapse = "") |> 
  strtoi(base=2)                  #844


#################################################################
#Part 2:

#To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position.

#function that analyzes a table (t) to get rating (oxygen or Co2) value.
get_rating <- function(rating, t){
  for(i in 1:ncol(t)){   
    onecount <- sum(t[,i])
    zerocount <- nrow(t)-onecount
    if((  rating == 'oxygen' & onecount  >= zerocount) 
       | (rating == 'Co2'    & zerocount >  onecount)){
      t <- t[t[,i]==1,]                                      #Only keep rows with 1 in this column.
    } else {
      t <- t[t[,i]==0,]                                      #Else keep rows with 0 in this column  
    }
    if(nrow(t)==1){
      break}                                                 #Stop when we have a single row left
  }
  paste(t, collapse = "")  %>% strtoi(base=2)                #Convert the remaining row to a decimal integer 
}
  
#Calculate the result
get_rating('oxygen',t) * get_rating('Co2',t)                # 6677951: Correct


