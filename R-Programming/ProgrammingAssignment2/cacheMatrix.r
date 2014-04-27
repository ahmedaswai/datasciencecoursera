
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


## This function will search for the matrix in the cache
# In case if It is in the cache it  return its inverse 
# else calculate the inverse and add it to the cache

cacheSolve <- function(x, ...) {
  al=makeCacheMatrix(x)   
  if(!is.null(al$get()))
  {
      print('Getting from the cache')
      return (al$get_inverse())
  }
  al$set(x)
  al$set_inverse(solve(x))
  return (al$lget_inverse())
}
