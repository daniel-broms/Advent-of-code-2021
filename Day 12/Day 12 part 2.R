#Day 12
library(stringr)
library(tidyverse)
library(profvis)

#read the paths into a table.
t <-read.table("Day 12/test.txt", col.names = c('From', 'To'), sep="-" )                     
t <-read.table("Day 12/input.txt", col.names = c('From', 'To'), sep="-" )                    

#Add opposite directions for all paths except Start and End nodes:
t <-rbind(t, t |> select(From=To, To=From))
t <- as.matrix(t)   #Changed to matrix for performance - filtering data.frame was too slow!


x <-find_paths("start", "start", vector(mode="character"), hasvisitedasmallcavetwice=FALSE)
length(x[!x==''])

#Return all paths from the given subpath. We cannot revisit visitedsmallcaves.
#Part 2 : We can revisit a single small cave twice.
find_paths <- function(node, subpath, visitedsmallcaves, hasvisitedasmallcavetwice ){
  
  #stop if we have reached 'end' : return this complete path as a string.
  if(node=='end'){
    return(subpath)
  }
  
  #if this is a small cave : Add to list of visited small caves.
  #Also record if we have visited a small cave twice.
  if(!node=='start' & substr(node,1,1)  %in% letters[1:26]){
    if(node %in% visitedsmallcaves) {hasvisitedasmallcavetwice <- TRUE}
    visitedsmallcaves <- c(visitedsmallcaves,node)
  }
  
  #Find all valid routes from the given node, explore each route.
  nextnodes <- t[t[,1]==node & !t[,2]=='start' & (!t[,2] %in% visitedsmallcaves | !hasvisitedasmallcavetwice) ,]
  if(length(nextnodes)==2){ nextnodes <-matrix(nextnodes, ncol=2) }             
  
  # Stop if we have reached a dead end. Return an empty string.
  if(length(nextnodes) == 0) return('')
  
  #call this function recursively to get all paths from the next nodes.
  allsubpaths <- vector(mode="character") 
  for(i in 1:nrow(nextnodes)){
    allsubpaths <- c(allsubpaths,find_paths(nextnodes[i,2], paste(subpath, nextnodes[i,2], sep=',' ), visitedsmallcaves, hasvisitedasmallcavetwice))
  }
  return(allsubpaths)
}



