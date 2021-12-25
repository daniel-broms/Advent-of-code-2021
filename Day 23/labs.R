state <- c(PA1="A2", PA2="D2", PB1="H10", PB2="C1", PC1="H10", PC2="C2", PD1="B2", PD2="D1")
getantipodmoves('PB2', state)


moveantipods(state)

#ordered dict keeps track of insertion order - not vaue order, so no good.
od <- collections::ordered_dict()

od$set('b', 2)
od$set('a', 1)
od$set('c', 3)

od$as_list()

#update a
od$set('a', 10)

od$pop()

#Propiry queue problem : We cannot update the priority (distance) once the item is in the queue.
q <- priority_queue()
q$push('a', 1)
q$push('b', 2)
q$push('c', 3)

q$as_list()
q$clear()


#Hash does not accept a character vector as key.
library(hash)
h <- hash()
h['a'] <- 1

values(h)

arrange()

#represent state as a string of 8 antipod positions
x <-str_c(state, collapse='')

#turn back into a state vector
sst <- strsplit(x, "")[[1]]
out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])


############################
#Dijkistra ideas:
############################
#Save visited nodes as a named vector with value=total distance from start. This list will be large and needs fast lookup.
#save "seen" (but not yet visited) nodes as a named vector

l <- c('a'=10, 'b'=2, 'c'=1, 'd' = 1)

#get the minimum distance
min(l)

#get the (first) seen state with minimum distance
names(l[l==min(l)][1]) 
#get the distance for a state, or check if a state exists (if so it returns NA)
l['xxx']

l[names(l) != "b"]

#speed test of finding if a state exists in the "explored" list:
explored_vector <- c(1:1000)
names(explored_vector)  <- str_c('State', as.character(explored_vector))
test1 <- function(name){ x <- explored_vector[name]; x}

h <- hash( str_c('State', as.character(explored_vector)) ,  explored_vector)
test2 <- function(name){ x <- h[[name]]; x}



microbenchmark(test1('State500'))   #mean = 3 microseconds- OK!
microbenchmark(test2('State500'))   #mean = 2.7 microseconds-about the same. Faster when n>10000.


test2('State5000')
seenstates


seenstates['asdasdasd'] > 10

moveantipods(state)


