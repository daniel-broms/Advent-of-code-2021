#Day 23 - What is the least energy required to organize the amphipods?
#Part 2 : 8 more rooms! Also needed to add more optimization in getantipodmoves() to reduce nr of possible states.

library(tidyverse)
library(collections)
library(microbenchmark)

#####################  initialize data #######################################
#State vector: Track which position each antipod is in.


state <- c(PA1="C4", PA2="D1", PB1="B1", PB2="D4", PC1="A4", PC2="B4", PD1="A1", PD2="C1")   #puzzle input => cost 41366 after 92000 iterations: correct!
#state <- c(PA1="A4", PA2="D4", PB1="A1", PB2="C1", PC1="B1", PC2="C4", PD1="B4", PD2="D1")  #test version => cost 44169 after 97000 iterations.

state <- c(state, PA3='C3', PA4='D2', PB3='B3', PB4='C2', PC3 ='B2', PC4 ='D3', PD3='A2', PD4='A3')  #Extra part 2 antipods
antipods <- names(state)

#Cost vector : How much each move costs per antipod type
cost <- c(A=1, B=10, C=100, D=1000)

#neighbor pairs. An antipod can move between these pairs.
P1 <- c('A2',        'A1',       'B2',        'B1',        'C2',        'C1',        'D2',       'D1',       'H1',       'H2',       'H3',       'H4',       'H5',       'H6',                'H7',       'H8',       'H9' ,      'H10')
P2<-  c('A1',        'H3',       'B1',        'H5',       'C1',        'H7',       'D1',       'H9',       'H2',       'H3',       'H4',       'H5',       'H6',       'H7',                'H8',       'H9',       'H10',     'H11')

#Part 2 new neighbor pairs
P1 <- c(P1, 'A4', 'A3', 'B4', 'B3', 'C4', 'C3', 'D4', 'D3')
P2 <- c(P2, 'A3', 'A2', 'B3', 'B2', 'C3', 'C2', 'D3', 'D2')

neighbors <-rbind(tibble(from=P1, to=P2), tibble(from=P2, to=P1))  #list both directions
neighbors<- as.matrix(neighbors)  #convert to matrix for speed

#####################  Execute  #######################################

moveantipods(state)


#####################  Functions #######################################
#Do a BFS to explore all possible moves from the given state. Accumulate the cost for each successive move. Stop when no further moves are possible and when we have explored all options.
moveantipods <- function(state){
  
  
  #Keep track of visited states and their distance, we do not want to revisit already visited states.
  #The keys are the state identifiers, the values are the total cost from starting state.
  visitedstates  <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
  seenstates     <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
  
  #Also hold a priority queue for seen but not visited states to efficiently get the next state to process.
  #Note that this queue will contain duplicates when a state distanace has been updated!
  seenstates_pq  <- priority_queue()
  
  #initialize seen states with the initial state and total cost 0.
  seenstates[[v2s(state)]] <-0
  seenstates_pq$push(v2s(state), 0)
  
  #Dijkstra's algorithm:
  #process seen states until we see no more states.
  iteration <- 0
  while(seenstates_pq$size() > 0){
    
    #Set the seen but not explored state which has the min total cost as the current state.
    currentstatestring <- seenstates_pq$pop()
    #TODO : Handle the case when the priority queue gets empty below! This will trigger an error, and we should break if the queue empties.
    while(exists(currentstatestring, envir=visitedstates )) currentstatestring <- seenstates_pq$pop()   #If we already have done this, get the next.
    
    currentstatecost   <- seenstates[[currentstatestring]]
    currentstate       <- s2v(currentstatestring) 
    iteration          <- iteration +1
    
    if(iteration %% 1000 == 0) print(c(iteration, currentstatecost))
    
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
          if(exists(candidatestatestring, envir=visitedstates)) break
          
          #if this state has been seen already, and the new total cost is lower than the existing total cost: Update the total cost.
          if(exists(candidatestatestring, envir=seenstates)){
            if(seenstates[[candidatestatestring]] > candidatestatecost) {
              seenstates[[candidatestatestring]] <- candidatestatecost
              seenstates_pq$push(candidatestatestring, -candidatestatecost)}      #Add this state again to the priority queue, now with a higher priority
            
          } else{
            #otherwise add this as a new seen state
            seenstates[[candidatestatestring]] <- candidatestatecost
            seenstates_pq$push(candidatestatestring, -candidatestatecost)
          }
        }
      } 
    }
    
    #Move the current state from seen to visited states.
    rm(currentstatestring, envir=seenstates)
    assign(currentstatestring,  currentstatecost, envir=visitedstates)
    
    
    #If this is a final state (all antipods are in their rooms): record the total cost and stop.
    if(sum(substr(currentstate,1,1) == substr(names(currentstate),2,2)) == length(antipods)){
      print(currentstatecost) #this is the puzzle answer
      break
    }
  }
}

#for the given antipod and state : Return all valid next destinations for this antipod, and the nr of steps to reach it.
getantipodmoves <- function(antipod, state){
  
  antipod_type <- substr(antipod,2,2)
  
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
    
    #Add check to not exit a correct room, or move upwards in the correct room
    if(str_sub(currentcell[1],1,1) == antipod_type && checkroom(currentcell[1], antipod, state )){   #If we are in the correct room type
      n <- n[str_sub(n,1,1) == antipod_type & str_sub(n,2,2) > str_sub(currentcell[1],2,2)]          #..then only include destinations in this room with higher index than current cell.
    } 
    
    #List each neighbor as explored and add them to the queue 
    for(i in seq_along(n)){
      exploredcells <- c(exploredcells, n[i])
      exploredcelldistances <- c(exploredcelldistances, as.integer(currentcell[2]) + 1)
      q$push( c(position = n[i], distance = as.integer(currentcell[2]) + 1))
    }
  }
  
  #List valid destinations given all rules, only return these destinations and the nr of steps to reach them (or maybe the the cost?)
  vd <- matrix(c(exploredcells,exploredcelldistances ), ncol=2)
  vd <- vd[ vd[,2] > 0, , drop=F]                                        #Starting position is not a valid destination
  vd <- vd[!vd[,1 ]  %in% c('H3','H5','H7','H9'), ,drop=F]               #Amphipods will never stop on the space immediately outside any room.
  if(str_sub(state[antipod],1,1)=='H'){                                  #If the starting position is in the hallway
    vd <- vd[str_sub(vd[,1 ],1,1) == antipod_type , , drop = F]  #...the only valid destination is in the correct room.
  }
  
  #If we can leave a room, do not include other positions in the room as valid options. We want to step out of the room in one step.
  if(any(str_sub(vd[,1],1,1) != str_sub(state[antipod],1,1))) {
    vd <- vd[str_sub(vd[,1 ],1,1) != str_sub(state[antipod],1,1) , , drop = F]
  }
  
  #If we are entering a room (from hallway or another room), only include the lowest available position in that rooms a valid destination in that room. We do not want to stop half way.
  #IOT : Remove destinations in the valid room other than the bottommost cell.
  bc <- max(vd[str_sub(vd[,1],1,1) == antipod_type ,1])                          #get the bottommost cell reachable in the destination room (if any)
  if(!is.na(bc)) {
    vd <- vd[!str_sub(vd[,1],1,1) == antipod_type | vd[,1] == bc , , drop = F]    #remove all destinaions in the room except the bottommost cell
  }
  
  #Do not move around in the starting room. Step out or stay still.
  vd <- vd[!str_sub(vd[,1],1,1) == str_sub(state[antipod],1,1) , , drop = F]     #remove all destinaions in the starting room
  
  return(vd)
}

#Check if a room is OK to enter. The room type must match the antipod type. Also, all room occupants must be of the correct type.
checkroom <- function(cell, antipod, state){
  roomtype <- str_sub(cell, 1,1)
  return(roomtype == str_sub(antipod, 2,2) && all(str_sub(names(state[str_sub(state,1,1) == roomtype]),2,2) == roomtype))   
}

#new statestring:H1-H7, R1-R8 (AABBCCDD)
v2s <- function(state){
  state2 <- c(H1='.', H2='.', H4='.', H6='.', H8='.', H10='.', H11='.', A1='.', A2='.', A3='.', A4='.', B1='.', B2='.', B3='.', B4='.', C1='.', C2='.', C3='.', C4='.', D1='.', D2='.', D3='.', D4='.')
  state2[state] <- str_sub(names(state),2,2)
  return(str_c(state2, collapse=''))
}

#repopulate current state from new statestring
s2v <- function(s){
  state2 <- unlist(str_split(s, pattern=''))
  names(state2) <- c('H1', 'H2', 'H4', 'H6', 'H8', 'H10', 'H11', 'A1', 'A2', 'A3', 'A4', 'B1', 'B2', 'B3', 'B4', 'C1', 'C2', 'C3', 'C4', 'D1', 'D2', 'D3', 'D4')
  state <- c(PA1=".", PA2=".",PA3=".", PA4=".", PB1=".", PB2=".", PB3=".", PB4=".", PC1=".", PC2=".", PC3=".", PC4=".", PD1=".", PD2=".", PD3=".", PD4=".")
  for(i in 1:length(state2)){
    if(!state2[i]=='.'){
      if(state[paste('P', state2[i], '1', sep='')] == '.')
      {
        state[paste('P', state2[i], '1', sep='')] <- names(state2[i])
      } else if(state[paste('P', state2[i], '2', sep='')] == '.') {
        state[paste('P', state2[i], '2', sep='')] <- names(state2[i])
      } else if(state[paste('P', state2[i], '3', sep='')] == '.') {
        state[paste('P', state2[i], '3', sep='')] <- names(state2[i])
      } else if(state[paste('P', state2[i], '4', sep='')] == '.') {
        state[paste('P', state2[i], '4', sep='')] <- names(state2[i])
      }
    }
  }
  return(state)
}













