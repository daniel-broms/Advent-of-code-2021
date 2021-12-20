#Day 17
#Test : target area: x=20..30, y=-10..-5
target_y1 <- -5
target_y2 <- -10
target_x1 <- 20
target_x2 <- 30

#Input: target area: x=81..129, y=-150..-108
target_y1 <- -108
target_y2 <- -150
target_x1 <- 81
target_x2 <- 129

##################
#Part 1 : Find maximum possible height.
#First find max(y) which will hit target y zone.
for(y in 1000:1){
  if(shoot(0,y)[4] >= target_y2) break #stop the first time we do not overshoot
}
yvel <- y  #our initial value : 149

#then find the first xvel which hits the target with this yvel.
for(x in 1:10000){
  if(shoot(x,yvel)[3] >= target_x1) break #stop the first time we overshoot
}
xvel <- x

#Shoot with this trajectory, for maxpos.
shoot(xvel, yvel)   #maxpos = 45. real input : 11175.

################################
#Part 2 : How many distinct initial velocity values cause the probe to be within the target area after any step?
#With test input : 
#yvel can be max +9 (from part 1) and min -11 (where we overshoot in the first step.)
#xvel must be between 1 and 31 (where we overshoot in the first step.)
#try all these combinations, see which hit.

hits<-list()
for(xvel in 1:31){
  for(yvel in -11:9){
    if(shoot2(xvel, yvel)[1] == 1){hits[[length(hits) + 1]] <- c(xvel, yvel) }
  }
}

hits<-list()
for(xvel in 1:target_x2){
  for(yvel in -target_y2:149){
    if(shoot2(xvel, yvel)[1] == 1){hits[[length(hits) + 1]] <- c(xvel, yvel) }
  }
}
length(hits) 

#Part 2 version
shoot2 <- function(xvel, yvel){
  pos <- c(0,0)
  maxypos <- 0
  repeat{
    #move to next position
    pos[1] <- pos[1] + xvel
    pos[2] <- pos[2] + yvel

    #record max ypos
    if(pos[2] >maxypos) maxypos <- pos[2]
    
    #drag
    if(xvel > 0) xvel <- xvel - 1
    if(xvel < 0) xvel <- xvel + 1
    
    #gravity
    yvel <- yvel - 1
    
    #Stop if we have passed the target.
    if(pos[2] < target_y2 | pos[1] > target_x2) break
    
    #Stop if we are in the target.
    if(pos[1] >= target_x1 & pos[1] <= target_x2 &  pos[2] <= target_y1 & pos[2] >= target_y2) break
    
  }
  #did we hit the target?
  if(pos[1] >= target_x1 & pos[1] <= target_x2 &  pos[2] <= target_y1 & pos[2] >= target_y2) {hit_taget <- 1}  else { hit_taget <- 0}
  return(c(hit_taget=hit_taget, maxypos= maxypos, finalx=pos[1], finaly=pos[2], xvel=xvel, yvel=yvel))
}


#Part 1 version
shoot <- function(xvel, yvel){
  pos <- c(0,0)
  maxypos <- 0
  repeat{
    #move to next position
    pos[1] <- pos[1] + xvel
    pos[2] <- pos[2] + yvel

    #record max ypos
    if(pos[2] >maxypos) maxypos <- pos[2]
    
    #drag
    if(xvel > 0) xvel <- xvel - 1
    if(xvel < 0) xvel <- xvel + 1
    
    #gravity
    yvel <- yvel - 1
    
    #Stop when y has reached target.
    if(pos[2] <= target_y1) break
  }
  #did we hit the target?
  #browser()
  if(pos[1] >= target_x1 & pos[1] <= target_x2 &  pos[2] <= target_y1 & pos[2] >= target_y2) {hit_taget <- 1}  else { hit_taget <- 0}
  
  return(c(hit_taget=hit_taget, maxypos= maxypos, finalx=pos[1], finaly=pos[2], xvel=xvel, yvel=yvel))
}

