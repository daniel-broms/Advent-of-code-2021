#day 5

#Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
#input <- readLines("Day 5/test.txt", )    #sample input
input <- readLines("Day 5/input.txt", )   #puzzle input

nlines <- length(input)  
maxcoord <- 1000

#format iput to a 4*10 matrix of integers.
l <- unlist(str_split(input, '->'))
l <- unlist(str_split(l, ','))
l <-as.integer(l)
dim(l) <- c(4,nlines)
l <- t(l)
colnames(l) <- c('x1','y1','x2','y2')
t<-data.frame(l)

#Keep horizontal or vertical lines x1 = x2 or y1 = y2.
t<- t[t$x1==t$x2 | t$y1==t$y2,]

#Render lines, find overlaps.
m<-matrix(0,nrow=maxcoord,ncol=maxcoord)

for(i in 1:nrow(t)){
  if(t[i,'x1'] == t[i,'x2']){
    xpos <- t[i,'x1']
    for(ypos in t[i,'y1']: t[i,'y2']){
      m[xpos+1, ypos+1 ] <- m[xpos+1, ypos+1 ] +1
    }
  } else {
    ypos <- t[i,'y1']
    for(xpos in t[i,'x1']: t[i,'x2']){
      m[xpos+1, ypos+1 ] <- m[xpos+1, ypos+1 ] +1
    }
  }

}

sum(m>1)  #7269:correct!



