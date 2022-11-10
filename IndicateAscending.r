

FindSorted <- function(v){
  
  s <- sort(v)
  index <- 0
  for(i in (1:length(v))){
    index <- index + 1
    if(!s[i]==v[i]){
      return(index)
    }
    
  }
  return(index)
  
}

v <- c(1,2,3,4,6,5,7)
ind <- FindSorted(v)


IndicateAscending <- function(v){
  
  
  a <- integer(length(v))
  a[1] <- 1
  a[length(a)] <- 1
  
  for(i in (2:(length(a)-1))){
    if((v[i+1]-v[i]) == 1){
      
      a[i] <- 1
      a[i+1] <- 1
    }
    
  }
  return(a)
  
}
v <- c(0,4,5,3,2,1,6,7,8)
a <- IndicateAscending(v)
