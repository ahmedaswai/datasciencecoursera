
# Function purpose: This function caches the matrix and find its cached inverse
# Params:matrix
#T Logic:The function is going to 
# returns cached matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <-NA
  set<-function(m)
  {
    m<-x
  }
  get <-function()
  {
     m
  }
  set_inverse<-function()
  {
     inv <- solve(x) 
    
  }
  get_inverse <-function()
  {
     if(is.na(inv))
     {
       set_inverse()
     }
      inv
    
  }
  list(get=get,set=set,set_inverse=set_inverse,get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    makeCacheMatrix(x)
}
