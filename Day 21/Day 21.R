#Day 21 :  multiply the score of the losing player by the number of times the die was rolled during the game
options(scipen=999)
#Simulate the game in Part 1.

#initialize
p1pos <- 3
p1score <- 0

p2pos <- 5
p2score <- 0

dice <- 0
rolls <- 0

while(p1score < 1000 & p2score<1000){
  #p1 rolls dice and moves
  dice_score <- (dice + 1) + (dice + 2) + (dice + 3)
  dice <- dice + 3
  rolls <- rolls + 3
  
  p1pos <- (p1pos + dice_score) %% 10
  p1score <- p1score + ifelse(p1pos ==0, 10, p1pos)
  if(p1score >= 1000) break
  
  #p2 rolls dice and moves 
  dice_score <- (dice + 1) + (dice + 2) + (dice + 3)
  dice <- dice + 3
  rolls <- rolls + 3
  
  p2pos <- (p2pos + dice_score) %% 10
  p2score <- p2score + ifelse(p2pos ==0, 10, p2pos)
  if(p2score >= 1000) break
  
}
rolls * min(p1score, p2score)



