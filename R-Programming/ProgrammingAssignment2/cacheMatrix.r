
# Function purpose: This function caches the matrix and find its cached inverse
# Params:matrix
#T Logic:The function is going to 
# returns cached matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <-NA
  set(x)
  set<-function(m)
  {
     x<-m
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
      inv
    
  }
  list(get=get,set=set,set_inverse=set_inverse,get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    makeCacheMatrix(x)
}
