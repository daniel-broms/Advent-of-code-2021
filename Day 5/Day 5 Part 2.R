#day 5 part 2:

#At how many points do at least two lines overlap?
input <- readLines("Day 5/test.txt", )    #sample input
#input <- readLines("Day 5/input.txt", )   #puzzle input


#format input to a 4-column matrix of integers.
l <- input        |> 
  str_split('->') |> 
  unlist()        |> 
  str_split( ',') |> 
  unlist()        |> 
  as.integer() 

dim(l) <- c(4,nlines)
l <- t(l)
colnames(l) <- c('x1','y1','x2','y2')

#Render lines, find overlaps.
nlines   <- length(input)  
maxcoord <- max(l) + 1
m        <- matrix(0,nrow=maxcoord,ncol=maxcoord)
yinc     <- 0

for(i in 1:nrow(t)){
  if(l[i,'x1'] == l[i,'x2']){
    
    #render a horizontal line
    xpos <- l[i,'x1']
    for(ypos in l[i,'y1']: l[i,'y2']){
      m[xpos+1, ypos+1 ] <- m[xpos+1, ypos+1 ] + 1
    }
  } else if(l[i,'y1'] == l[i,'y2']) {
    
    #render a vertical line
    ypos <- l[i,'y1']
    for(xpos in l[i,'x1']: l[i,'x2']){
      m[xpos+1, ypos+1 ] <- m[xpos+1, ypos+1 ] + 1
    }
  } else {
    
    #render a diagonal line
    ypos <- l[i,'y1'] 
    if(t[i,'y1'] > t[i,'y2']) {yinc <- -1} else {yinc <- 1}
    
    for(xpos in l[i,'x1']: l[i,'x2']){
      m[xpos+1, ypos+1 ] <- m[xpos+1, ypos+1 ] + 1
      ypos <- ypos + yinc
    }
  }
}

sum(m>1)  #12 for test input, 21140 for real input:  correct!

