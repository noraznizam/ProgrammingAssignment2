## Put comments here that give an overall description of what your
## functions that cache the inverse of a matrix


## This function creates a special kind of matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
  ## to initializing the inverse property
  inv <- NULL
  
  ## method to set the matrix
  set <- function(y)
  {
    matrix <<- y
    inv <<- NULL
  }
  
  ## method to get the matrix
  get <- function()
  {
    ## returning matrix
    matrix
  }
  
  ## method to set the inverse of the matrix
  setInverse <- function(inverse) 
  {
    ## storing inverse 
    inv <<- inverse
  }
  
  ## method to get the inverse of the matrix
  getInverse <- function() 
  {
    ## returns the inverse
    inv
  }
  
  ## Returns the list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## - If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## getting a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ################################################
  ## If the inverse wasn't yet been calculated ##
  ################################################
  
  ## getting the matrix from our object
  data <- x$get()
  
  ## calculating the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## storing the inverse to the object to future usage
  x$setInverse(m)
  
  ## returning a matrix that is the inverse of 'x'
  m 
}
