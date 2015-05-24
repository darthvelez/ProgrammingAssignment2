##Create a two fuctions where:

## this first function returns creates a list with the following Functions: 

#  1: set      set the value of a matrix
#  2: get      get the value of a matrix
#  3: cachesolve   get the cached value 
#  4: getsolve     get the cached value 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  #1
  set <- function(Matrix){
    x <<- Matrix
    inv <<- NULL
  }
  
  #2
  get <- function() {
    x
  }
  
  #3
  setsolve <- function(solve) {
    inv <<- solve
  }
  
  #4
  getsolve <- function() {
    inv
  }
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}

## this second function Returns the inverse of the matrix created with "makeCacheMatrix"

cachesolve <- function(x, ...){
  
  inv <- x$getsolve()
  
  #if the inverse of the matrix is already stored in the cache get it
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if the inverse of the matrix doesn`t already exist, make the calculation from the stored original matrix
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  
  print (inv)
}
