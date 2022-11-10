

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
