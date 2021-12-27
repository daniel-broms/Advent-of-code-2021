state <- c(PA1="A2", PA2="D2", PB1="H10", PB2="C1", PC1="H10", PC2="C2", PD1="B2", PD2="D1")
getantipodmoves('PB2', state)


moveantipods(state)


#TODO : Imrove heuristic to only move downwards in a room with only correct antipod types. NO : Such a move might be needed!!! 
#check if the room only contains antipods of the correct type:
checkroomoccupants <- function(antipod, state){
  atype <- str_sub(antipod,2,2)
  
}

#list the antipod types in a given room
str_sub(names(state[str_sub(state,1,1) == 'A']),2,2)
