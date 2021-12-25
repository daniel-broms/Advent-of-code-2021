#Day 21 : Find the player that wins in more universes; in how many universes does that player win?
options(scipen=999)

########################## Part 2 ################################
#Every time the dice is rolled, each universe is split into three universes, which quickly becomes quite many. 
#Keep track of games in a 4-dimensional array (P1pos, p1points, p2pos, p2points) where the value = nr of games in this state.

#Game statesarray, 48400 different states! R array indexes are one-based, so index = (points + 1). 
#Points never go beyond 21 (index 22) and pos is always in range 1-10.
m <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))  
p1_wongames <- 0
p2_wongames <- 0

#starting position : a single game, zero-zero. P1 starts at 4, P2 starts at 8.
#m[4,1,8,1] <- 1   #test input
m[3,1,5,1] <- 1   #puzzle input


while(sum(m) > 0){
  ######################################## P1 ##############################
  #P1 rolls the dice three times
  for(i in 1:3){
    #P1 rolls dice : Copy "nr of games" three times to new matrix, at wrap(p1pos+1), wrap(p1pos+2), wrap(p1pos+3).
    #first outcome : P1 rolls 1
    m1 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m1[wrap(p1pos+1), p1points, p2pos, p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    #second outcome : P1 rolls 2.
    m2 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m2[wrap(p1pos+2), p1points, p2pos, p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    #third outcome : P1 rolls 3.
    m3 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m3[wrap(p1pos+3), p1points, p2pos, p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    m <- m1+m2+m3  #Combine the universes
  }
  
  #P1 collects points for the last round. Move all games to the new points position.
  m4 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22)) 
  for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m4[p1pos, wrappoints(p1points + p1pos), p2pos, p2points] <- m4[p1pos, wrappoints(p1points + p1pos), p2pos, p2points] + m[p1pos, p1points, p2pos, p2points]}}}}
  m <- m4
  
  #Record and remove all games which P1 has now won  = m[,22,,]
  p1_wongames <- p1_wongames + sum(m[,22,,])
  m[,22,,] <- 0
  
  ######################################## P2 ##############################
  #P2 rolls the dice three times
  for(i in 1:3){
    #P2 rolls dice : Copy "nr of games" three times to new matrix, at wrap(p1pos+1), wrap(p1pos+2), wrap(p1pos+3).
    #first outcome : P2 rolls 1
    m1 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m1[p1pos, p1points, wrap(p2pos+1), p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    #second outcome : P2 rolls 2.
    m2 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m2[p1pos, p1points, wrap(p2pos+2), p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    #third outcome : P2 rolls 3.
    m3 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22))
    for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m3[p1pos, p1points, wrap(p2pos+3), p2points] <- m[p1pos, p1points, p2pos, p2points]}}}}
    
    m <- m1+m2+m3  #Combine the universes
  }
  
  #P2 collects points for the last round. Move all games to the new points position.
  m4 <- array(0L, c(p1pos=10, p1points =22, p2pos=10, p2points=22)) 
  for(p1pos in 1:10){  for(p1points in 1:22){ for(p2pos in 1:10){ for(p2points in 1:22)   {m4[p1pos, p1points , p2pos, wrappoints(p2points + p2pos)] <- m4[p1pos, p1points, p2pos, wrappoints(p2points + p2pos)]  + m[p1pos, p1points, p2pos, p2points]}}}}
  m <- m4
  
  #Record and remove all games which P2 has now won  = m[,,,22]
  p2_wongames <- p2_wongames + sum(m[,,,22])
  m[,,,22] <- 0
}

p2_wongames  #180 790 203 020 533
p1_wongames  #275 067 741 811 212

################ Functions #########################
wrap    <- function(pos){  if(pos > 10 ) pos <- pos-10; pos}
wrappoints <- function(points) min(points,22)



