

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

BreakPointSort <- function(vec){
  v <- c(0, vec, (max(vec)+1))
  s <- sort(v)
  dis <- 0
  while(TRUE){
    dis <- dis + 1
    # Mark ascending descending parts
    #print(paste("V :", toString(v)))
    a <- IndicateAscending(v)
    
    d_values <- v[which(a %in% 0)]
    # If there is no descending vector we must reverse everything
    # between FindSorted and the last bp
    if(length(d_values)==0){
      v <- c(v[1:(FindSorted(v)-1)], rev(v[FindSorted(v):(length(v)-1)]), max(v))
      # We recalculate the descending parts
      a <- IndicateAscending(v)
      d_values <- v[which(a %in% 0)]
    }
    # If we only have one descending element and only the sorted path is in front
    # of it we must reverse everything between FindSorted(v) and the las bp
    if(length(d_values) == 1 && (which(v == min(d_values)) == FindSorted(v))){
      v <- c(v[1:(FindSorted(v)-1)], rev(v[FindSorted(v):(length(v)-1)]), max(v))
      # We recalculate the descending parts
      a <- IndicateAscending(v)
      d_values <- v[which(a %in% 0)]
    }
    #print(paste("d_values :", toString(d_values)))
    # Take the index of the min value in the descending parts
    rev_bp <- which(v == min(d_values))
    #Reverse sequence between first bp and min rev_bp
    v <- c(v[1:(FindSorted(v)-1)], rev(v[FindSorted(v):rev_bp]), v[(rev_bp+1):length(v)])
    if(identical(v,s)){
      print(paste("Sorted Sequence :", toString(v[2:(length(v)-1)])))
      print(paste("Distance :", toString(dis)))
      return(v[2:(length(v)-1)])
      
    }
  }
  
}

v <- c(4,5,3,2,1,6,7,8)
s <- BreakPointSort(v)





