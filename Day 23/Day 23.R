#Day 23 - What is the least energy required to organize the amphipods?
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
#TODO : Use Dijkstras algorithm in "moveantipods".
#TODO :Set up pairs as hastable or matrix (instead of tibble) for faster processing

#####################  initialize data #######################################
#State vector: Track which position each antipod is in.
state <- c(PA1="A2", PA2="D2", PB1="A1", PB2="C1", PC1="B1", PC2="C2", PD1="B2", PD2="D1")
antipods <- names(state)

#Cost vector : How much each move costs per antipod type
cost <- c(A=1, B=10, C=100, D=1000)

#neighbor pairs. An antipod can move between these pairs.
P1 <- c('A2',	'A1',	'B2',	'B1',	'C2',	'C1',	'D2',	'D1',	'H1',	'H2',	'H3',	'H4',	'H5',	'H6',	'H7',	'H8',	'H9' ,	'H10')
P2<-  c('A1',	'H3',	'B1',	'H5',	'C1',	'H7',	'D1',	'H9',	'H2',	'H3',	'H4',	'H5',	'H6',	'H7',	'H8',	'H9',	'H10',	'H11')
neighbors <-rbind(tibble(from=P1, to=P2), tibble(from=P2, to=P1))  #list both directions


#####################  Execute  #######################################

#put PB2 in H1, leaving C1 empty, check that PC1 can enterC1.
#state <- c(PA1="A2", PA2="D2", PB1="H2", PB2="H1", PC1="B1", PC2="C2", PD1="B2", PD2="D1")
#getantipodmoves('PC2', state)
#checkroom('A1','PA2', state )

#moveantipods(state)

microbenchmark(getantipodmoves('PB1',state),  times=10)# Mean = 104 milliseconds : About 0.1 seconds per call, not good!

#####################  Functions #######################################
#Do a BFS to explore all possible moves from the given state. Accumulate the cost for each successive move. Stop when no further moves are possible and when we have explored all options.
moveantipods <- function(state){
  
  #Initialize the queue with the current state and accumulated cost = 0.
  q <- collections::queue()  
  q$push(list(state,0))
  
  #Keep track of explored states, we do not want to revisit already explored states. NO : Rules prevent loops. Also, we want to visit the final state many times!
  #exploredstates <- dict()
  #exploredstates$set(state, 0)  
  
  while(q$size() > 0){
    currentstatelist <- q$pop()   #Process the next possible state
    currentstate <- currentstatelist[[1]]
    currentcost  <- currentstatelist[[2]]
    
    #Get all possible next states from the current state, add them to the queue.
    for(i in 1:8){
      t <- getantipodmoves(antipods[i], currentstate)
      for(j in seq_len(NROW(t))){
        candidatestate <- currentstate
        candidatestate[antipods[i]] <- t[[j,'cell']]
        candidatestatecost <- currentcost + t[[j,'distance']] * cost[substr(antipods[i],2,2)]
        q$push(list(candidatestate,candidatestatecost))
      }
    }
    
    print(c(currentstate, currentcost))
    
    #If this is a final state (all antipods are in their rooms): record the total cost.
    if(sum(sum(substr(currentstate,1,1) == substr(names(currentstate),2,2))) == 8){
      print(currentcost)
    }
  }
}

#for the given antipod and state : Return all valid next destinations for this antipod, and the nr of steps to reach it.
getantipodmoves <- function(antipod, state){
 
   #Return nothing if the antipod is in its correct room at pos2, or pos1 if the correct antipod is in pos2. This is the final state.
  if( state[antipod]  == str_c(substr(antipod,2,2), '2') |        
     (state[antipod]  == str_c(substr(antipod,2,2), '1')) && str_sub(names(state[state==str_c(substr(antipod,2,2), '2')]),2,2) == substr(antipod,2,2)){
    return(tibble(cell=c(), distance = c()))
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
  n <-neighbors                     |> 
    filter(from == currentcell[1])  |>                 #Neigbors reachable from current cell
    filter(! to %in% exploredcells) |>                 #...which have not yet been explored
    filter(! to %in% state)         |>                 #...and are not occupied by another antipod
    filter(str_sub(to,1,1) == 'H'|                     #...and is either in the hall 
           checkroom(to, antipod, state)) |>           #...or in a good room
    pull(to)                                       

    #List each neigbbor as explored and add them to the queue 
    for(i in seq_along(n)){
      exploredcells <- c(exploredcells, n[i])
      exploredcelldistances <- c(exploredcelldistances, as.integer(currentcell[2]) + 1)
      q$push( c(position = n[i], distance = as.integer(currentcell[2]) + 1))
    }
    
  }
  #List valid destinations given all rules, only return these detinations and the nr of steps to reach them (or maybe the the cost?)
  vd <- tibble(cell=exploredcells, distance=exploredcelldistances)
  vd <- filter(vd, !distance==0)                                 #Starting position is not a valid destination
  vd <- filter(vd, !cell %in% c('H3','H5','H7','H9'))            #Amphipods will never stop on the space immediately outside any room.
  if(str_sub(state[antipod],1,1)=='H'){                          #If the starting position is in the hallway, t
   vd <- filter(vd, str_sub(cell,1,1) == str_sub(antipod,2,2))  #...the only valid destination is in the correct room.
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




