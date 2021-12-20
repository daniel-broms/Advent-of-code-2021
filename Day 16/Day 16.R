#Day 16
library(tidyverse)
library(R.utils)

table <- read_fwf("Day 16/table.txt",col_positions = fwf_widths(c(1,3,4)) )
input <- readLines("Day 16/input.txt", )   #puzzle input

#For now, parse the hierarchy of the packets throughout the transmission and add up all of the version numbers.


#input <- 'A0016C880162017C3686B18A3D4780'  
input <- str_c(hextobin(input), collapse='') #Convert to a binary string
parsepacket(input)

#Recurively process a packet and contained sub-packets. Sum up version number.
parsepacket <- function(packet){
  
  #If only zeroes remain : return zero and empty string.
  if(nchar(packet) < 10 & strtoi(packet, base = 2)==0) return(list(0, ''))
  
  #Get packet version : first three bits
  pv <- strtoi(str_sub(packet,1,3), base = 2)
  #Get packet type : next three bits
  pt <- strtoi(str_sub(packet,4,6), base = 2)
  
  #If the type = 4 it is a literal value. It will be followed by groups of 5 bits, where the last group begins with a 0. Then there will be 0 padding so make the numbers bits  av even multiple of 4.
  #In part 1 : Ignore the values, just return the packet version (which will be summed) and the remaining string after this packet as been removed.
  #browser()
  if(pt==4){
    pos <- 7  #begin at position 7
    while(!str_sub(packet, pos, pos) == "0"){pos <- pos + 5}  #find the sequence of five bits which begins with a zero, indicating the last sequece
    pos <- pos + 5                                            #skip the last sequence, point to the start of the next package (the rest of the string)
    return(list(pv, str_sub(packet,pos)))                     #return the version (to sum up) and the rest of the string.
  }
  
  #If the type !=4 it is an operator. Contains one or more sibling subpackets. Read the next bit to get the "length type bit".
  ltb <- str_sub(packet,7,7)
  #    If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet
  #    TODO : Extract each of these sub-packets, recursively parse each one via parsepacket(). 
  if(ltb == "0"){
    sub_packet_totlength <- strtoi(str_sub(packet, 8, 8+14), base = 2)  # the 15 bits in positions 8-22 hold the total length of sub-packets
    packet <- str_sub(packet, 23)                                       # remove the current package (is always 22 bits long)  
    start_length <- nchar(packet)                                       # the initial length
    
    #call parsepacket recursively until we have chopped of sub_packet_totlength bits from "packet".
    while(nchar(packet) > (start_length - sub_packet_totlength )){
      rv <- parsepacket(packet)
      packet<- rv[[2]]
      pv <- pv + rv[[1]]
    }
    return(list(pv,packet ))
  } else {
    #    If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
    #    TODO : Extract these sub-packets, recursively parse each one via parsepacket().
    sub_packet_count <- strtoi(str_sub(packet, 8, 18), base = 2)        #the 11 bits in positions 8-18 hold the total number of sibling sub-packets.
    packet <- str_sub(packet, 19)                                       # remove the current package (is always 18 bits long)
    
    #call parsepacket recursively sub_packet_count times
    for(i in 1:sub_packet_count){
      rv <- parsepacket(packet)
      packet<- rv[[2]]
      pv <- pv + rv[[1]]
    }
    return(list(pv,packet ))
  }
}

#Part 2:
#pt types:
#0=sum
#1=product
#2=min
#3=max
#5=greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
#6=less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
#7=equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

################# Utility functions #################
hextobin <- function(s){
  bin <- vector()
  for(i in 1:nchar(s)){
    bin <- c(bin, table[table$X1==str_sub(s,i,i),][[3]])
  }
  return(bin)
}

