#Day 19
library(collections)
library(tidyverse)
# By finding pairs of scanners that both see at least 12 of the same beacons, you can assemble the entire map.
# Assemble the full map of beacons. How many beacons are there?
input <- readLines("Day 19/test.txt", )    #sample input
nrscanners <- 5

input <- readLines("Day 19/input.txt", )   #puzzle input
nrscanners <- 38


#Save scanners as a list of 3D-arrays.
startrow <- 2
scanners <-list()
for(i in 1:nrscanners){
  endrow <- startrow
  while(endrow <= length(input) &&  !input[endrow] == '') endrow <- endrow+1
  a <- as.integer(unlist(str_split(input[startrow:(endrow-1)],',')))
  a <- array(a, dim=c(3,endrow-startrow))
  a <- t(a)
  scanners[[length(scanners)+1]] <- a

  startrow <- endrow + 2
}

#Compare scanners in order of discovery until all scanners have been visited.
#Do not compare with already seen or checked scanners.
#When an overlapping scanner has been seen : save its distance vector to the first scanner, and save the rotated beacon positions
checkedscanners <- vector()
seenscanners    <- c(1L)
distances       <- dict()     #dictionary with distance vector for each scanner. Key=scanner index.

#Initialize with the first scanner, then pick among seen but not yet checked scanners.
currentscanner  <- 1
currentdistance <- c(0,0,0)
distances$set(1L, c(0,0,0))
beacons         <- scanners[[1]]

while(length(checkedscanners)< nrscanners){
  for(i in 1:nrscanners){
    if(!i == currentscanner & !i %in% checkedscanners & !i %in% seenscanners){
      cb <- comparescanners(scanners[[currentscanner]], scanners[[i]])
      if(cb[[1]] ==T){ 
        #browser()
        print(c(currentscanner, i, cb[[2]]))
        seenscanners <- c(seenscanners, i)
        scanners[[i]] <- cb[[3]]                               #update the beacons list with the correctly oriented beacons 
        beacons <- rbind(beacons, cb[[3]] + currentdistance + cb[[4]]  )    #record rotated beacons adjusted to their distance to origo (this does not work! unclear why...)
        distances$set(i,currentdistance + cb[[4]] )            #record the accumulated distance of this scanner to origo
      }
    }
  }
  checkedscanners <- c(checkedscanners, currentscanner)        #Mark this scanner as checked
  seenscanners <- seenscanners[!seenscanners==currentscanner]  #..and remove it from seen but not checked scanners
  currentscanner <- seenscanners[1]                            #Pick the next scanner to check.
  if(!is.na(currentscanner)){
    currentdistance <- distances$get(currentscanner)            #...and get its total distance to origo
  }
}
#When we have found the correct orientation of all scanners, and recorded the total distance of all scanners to origo, we can make a global beacon list and remove duplicates.
#for each scanner : get distance. For each (rotated) beacon : Add distance. 
m <- matrix(rep(0,length(beacons) ), ncol =3)
row <- 1
for(i in 1L:nrscanners){
  sd <- distances$get(i)
  for(j in 1:dim(scanners[[i]])[1]){
    print(scanners[[i]][j,] + sd)
    m[row, ] <- scanners[[i]][j,] + sd
    row <- row+1
  }
}

#Count the number of unique beacons.
dim(unique(m))[1]   #79 in test input;  462 with real input : Correct!

##############################
##### Part 2 ################
#What is the largest Manhattan distance between any two scanners?
dl <- distances$as_list()
maxdist <- 0
for(i in 1:nrscanners){
  for(j in 1:nrscanners){
    dist <- sum(abs(dl[[i]] -dl[[j]]))
    if(dist> maxdist) maxdist <- dist
  }
}
maxdist   #12158 : correct!

############################################### Functions #########################################################################
#Compare two given scanners. Rotate the second scanner in all 24 possible orientations, compare each orientation.
comparescanners <- function(s1, s2){
rotation <- 1
  repeat{
    cb <- findcommonbeacons(s1,s2)
    if(cb[[1]]==T) {
      break                  #Stop if we find at least 10 overlapping beacons.
    }
    if(rotation %in% c(1,2,3, 5,6,7 , 9,10,11, 14,15,16, 18,19,20, 22,23,24)) s2 <- t(apply(s2, MARGIN=1, rotx))
    if(rotation %in% c(4,8,12, 17,21,25))                                     s2 <- t(apply(s2, MARGIN=1, roty))
    
    if(rotation == 13){
      #flip
      s2 <- t(apply(s2, MARGIN=1, rotx))
      s2 <- t(apply(s2, MARGIN=1, roty))
      s2 <- t(apply(s2, MARGIN=1, rotx))
    }
    rotation <- rotation + 1
    if (rotation>25) break  #Stop after 25 rotations
  }
  return(list(matchfound = cb[[1]], 
              rotation   = rotation, 
              beacons    = s2, 
              distance   = cb[[2]]))
}

#Return the beacons in S1 which are in common with s2, i.e. they have identical differences to each other.
findcommonbeacons <- function(s1, s2){
  #list all differences for all combinations of beacons, see how many are identical.
  df <- data.frame(matrix(0, nrow = dim(s1)[1] * dim(s2)[1],ncol = 5 ))
  colnames(df) <- c('s1b','s2b','dx','dy','dz')
  row <- 1
  for(i in 1:dim(s1)[1]){
    for( j in 1:dim(s2)[1]){
      df[row, 1:2] <- c(i,j)
      df[row, 3:5] <- s1[i,] - s2[j,]
      row <- row + 1
    }
  }
  
  #find the duplicates (note that the first is not included since it is not yet duplicated!)
  dup <- df[which(duplicated(df[,3:5])),]
  
  #find the distance vector(s) which occurs at least 10 times:
  dup <- group_by(dup, dx, dy,dz ) %>% summarize(n=n(), .groups="keep") %>% filter(n>10) %>% ungroup()
  
  #find the beacon pairs which match this distance vector
  if(nrow(dup) == 1) { 
    df <- df[ (df$dx == dup[[1,'dx']] &  df$dy == dup[[1,'dy']] &  df$dz == dup[[1,'dz']]) ,]
    return(list(matchfound = T, distance = c(dup[[1,'dx']], dup[[1,'dy']], dup[[1,'dz']])))  
  }
  else
    return(list(matchfound = F, distance = NULL))
}

#rotate around x axis:
#x,y,z -> x,-z,y
rotx <- function(coo){
  return(c(coo[1], -coo[3], coo[2]))
}

#rotate around y axis:
#x,y,z -> z,y,-x
roty <- function(coo){
  return(c(coo[3], coo[2], -coo[1]))
}


