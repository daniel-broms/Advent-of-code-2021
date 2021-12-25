#Day 23 - What is the least energy required to organize the amphipods?
library(tidyverse)
library(collections)
library(microbenchmark)

#The hall has 11 spaces, but the four at doors are blocked, so only 7 are legal destinations.
#All possible first moves are: 
#  move B, C,C,D  to any of the 7 spaces : 4*7=28 options to explore. enque all these options and visit them one by one, in order of cost.
#  when visiting each option : find all options from this point, following the rules.
#  For each node : Calculate the total cost to get to this state as the accumulated cost + cost to do the next move.

#There are 8 antipods (A1, A2..D2)
#There are 15 positions where an antipod can be: RA1, RA2, RB1...RD2, H1, H2..H7.
#There is a distance (in steps) between each of these positions, can be represented as a 15*15 matrix.
#There is route between position neighbors indicating which neighbors can be reached from which position. 
#(There is a route between each of these positions, can be represented as the positions, in order, which need to be traversed to reach the destination.)

#The current state can be represented as a named vector of 8 showing in which of the 15 positions each antipod is, and the accumulated cost to reach this state. 

#####################  initialize data #######################################
#State vector: Track which position each antipod is in.

state <- c(PA1="C2", PA2="D1", PB1="B1", PB2="D2", PC1="A2", PC2="B2", PD1="A1", PD2="C1")   #puzzle input
#state <- c(PA1="A2", PA2="D2", PB1="A1", PB2="C1", PC1="B1", PC2="C2", PD1="B2", PD2="D1")  #test version => 12521
antipods <- names(state)

#Cost vector : How much each move costs per antipod type
cost <- c(A=1, B=10, C=100, D=1000)

#neighbor pairs. An antipod can move between these pairs.
P1 <- c('A2',	'A1',	'B2',	'B1',	'C2',	'C1',	'D2',	'D1',	'H1',	'H2',	'H3',	'H4',	'H5',	'H6',	'H7',	'H8',	'H9' ,	'H10')
P2<-  c('A1',	'H3',	'B1',	'H5',	'C1',	'H7',	'D1',	'H9',	'H2',	'H3',	'H4',	'H5',	'H6',	'H7',	'H8',	'H9',	'H10',	'H11')
neighbors <-rbind(tibble(from=P1, to=P2), tibble(from=P2, to=P1))  #list both directions
neighbors<- as.matrix(neighbors)  #convert to matrix for speed

#####################  Execute  #######################################





#microbenchmark(getantipodmoves('PB1',state),  times=1000, unit='ms') # Mean = 8 milliseconds, 50x faster with neighbors as matrix than tibble!
#getantipodmoves('PB1',state)                                                                   # mean = 0.4 milliseconds when the second DF operation is removed. 



#####################  Functions #######################################
#Do a BFS to explore all possible moves from the given state. Accumulate the cost for each successive move. Stop when no further moves are possible and when we have explored all options.
moveantipods <- function(state){
  

  #Keep track of visited states and their distance, we do not want to revisit already visited states.
  #The names are the state identifiers, the values are the total cost from starting state.
  visitedstates  <- c(NA)
  seenstates     <- c()
  
  #initialize seen states with the initial state and total cost 0.
  seenstates[v2s(state)] <- 0
  
  #Dijkstra's algorithm:
  #process seen states until we see no more states.
  while(length(seenstates) > 0){
    
    #Set the seen but not explored state which has the min total cost as the current state.
    currentstatestring <- names(seenstates[seenstates==min(seenstates)][1])
    currentstatecost   <- seenstates[currentstatestring]
    currentstate <- s2v(currentstatestring) 
    #print(currentstate)

    #Get all possible next states from the current state, add them to seenstates
    for(i in 1:length(antipods)){
      t <- getantipodmoves(antipods[i], currentstate)
      if(length(t)>0){      
        for(j in 1:length(t)/2){
             
          #Calculate the candidate state vector and total cost to reach this state.
          candidatestate <- currentstate
          candidatestate[antipods[i]] <- t[[j,1]]
          candidatestatestring <- v2s(candidatestate)
          candidatestatecost <- currentstatecost + as.integer(t[[j,2]]) * cost[substr(antipods[i],2,2)]
          
          #if this state has been explored already, skip it.
          if(!is.na(visitedstates[candidatestatestring])) break
          
          #if this state has been seen already, and the new total cost is lower than the existing total cost: Update the total cost.
          if(!is.na(seenstates[candidatestatestring])){
            if(seenstates[candidatestatestring] > candidatestatecost) seenstates[candidatestatestring] <- candidatestatecost
          } else{
            #otherwise add this as a new seen state
            seenstates[candidatestatestring] <- candidatestatecost
          }
        }
      } 
    }
    
    #Move the current state from seen to visited states.
    seenstates <- seenstates[names(seenstates) != currentstatestring]
    visitedstates[currentstatestring] <- currentstatecost


    #If this is a final state (all antipods are in their rooms): record the total cost.
    if(sum(substr(currentstate,1,1) == substr(names(currentstate),2,2)) == 8){
      print(currentstatecost) #12521 for test problem, 
    }
  }
  print('ERROR : We should not get here.')
}

#for the given antipod and state : Return all valid next destinations for this antipod, and the nr of steps to reach it.
getantipodmoves <- function(antipod, state){
 
   #Return nothing if the antipod is in its correct room at pos2, or pos1 if the correct antipod is in pos2. This is the final state. Added sum check to see if lower room is occupied.
  if( state[antipod]  == str_c(substr(antipod,2,2), '2') |        
     (state[antipod]  == str_c(substr(antipod,2,2), '1')) && sum(state==str_c(substr(antipod,2,2), '2'))>0  && str_sub(names(state[state==str_c(substr(antipod,2,2), '2')]),2,2) == substr(antipod,2,2)){
    return(matrix(ncol=2,nrow=0))  #return an empty matrix
  }
  
  #Do a non-recursive BFS to find all reachable destinations.
   #initialize q with the current position
  valid_destinations <- c()

  q <- collections::queue()
  currentcell <- c(position = state[antipod], distance=0)     #TODO: distance is converted to a string since position is string, make a better tuple for position, distance.
  q$push(currentcell)             
  exploredcells         <- c(state[antipod])
  exploredcelldistances <- 0
  while(q$size() > 0){
    currentcell <- q$pop()  #Process the next reachable neighbor
    
  #Get reachable neighbors which we have not yet explored and are not occupied.
  n <-neighbors[ neighbors[,'from'] == currentcell[1] &  
                !neighbors[,'to']  %in% exploredcells &
                !neighbors[,'to']  %in% state 
                ,'to']
  
  #Add check for hall or correct room. 
  if(str_sub(currentcell[1],1,1) == 'H'){        #If we are coming from the hall
    for(i in seq_along(n)){
      if(!str_sub(n[i],1,1)=='H'){               #If the destination is not in the hall
        if(!checkroom(n[i], antipod,state)){     #..and the room is not ok
          n <- n[-i]                             #..then remove this as a reachable neighbor  
          break                                  #Stop here, or else we get errors since n has been shortened.
        }
      }
    } 
  }

 #List each neigbbor as explored and add them to the queue 
  for(i in seq_along(n)){
    exploredcells <- c(exploredcells, n[i])
    exploredcelldistances <- c(exploredcelldistances, as.integer(currentcell[2]) + 1)
    q$push( c(position = n[i], distance = as.integer(currentcell[2]) + 1))
  }
    
  }
  #List valid destinations given all rules, only return these detinations and the nr of steps to reach them (or maybe the the cost?)
  #TODO : Do this in a matrix also. Return a two-column matrix with columns cell and distance.
  vd <- matrix(c(exploredcells,exploredcelldistances ), ncol=2)
  vd <- vd[ vd[,2] > 0, , drop=F]                                        #Starting position is not a valid destination
  vd <- vd[!vd[,1 ]  %in% c('H3','H5','H7','H9'), ,drop=F]              #Amphipods will never stop on the space immediately outside any room.
  if(str_sub(state[antipod],1,1)=='H'){                          #If the starting position is in the hallway
    vd <- vd[str_sub(vd[,1 ],1,1) == str_sub(antipod,2,2) , , drop = F]    #...the only valid destination is in the correct room.
  }
  return(vd)
}


#Check if a room is OK to enter. The room type must match the antipid type. Also, either lower position is empty or it contains an antipod of the correct typ.
checkroom <- function(cell, antipod, state){
  roomtype <- str_sub(cell, 1,1)
  lowpos <- str_c(roomtype, '2')
  occupant <- names(state[state==lowpos])
  return(roomtype == str_sub(antipod, 2,2)) && (length(occupant)==0 || str_sub(occupant,2,2) == str_sub(cell,1,1))
}

#turn a state string into a vector with 8 members, one per antipod
s2v <- function(x){
  #turn back into a state vector
  sst <- strsplit(x, "")[[1]]
  rv <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
  names(rv) <- antipods
  rv[rv=='Hx'] <- 'H10'
  rv[rv=='He'] <- 'H11'
  return(rv)
}

#Turn a state vector to a single string used to index a state.
#quickfix : replace H10 with Hx, H11 with He. Reverse this when parsing string.
v2s <- function(x){
  x[x=='H10'] <- 'Hx'
  x[x=='H11'] <- 'He'
  str_c(x, collapse='')
}


