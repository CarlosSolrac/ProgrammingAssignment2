## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This follows the same algorithm for cacheMean
makeCacheMatrix <- function(x = matrix()) {
  ## init data
  m <- NULL
  
  ## sets the matrix and sets the cached inverse to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the current matrix
  get <- function() x
  
  ## This actually caches the inverse matrix in a separate environment
  setInverse <- function(inverse) m <<- inverse
  
  ## get the inverse matrix. If the inverse has not been calculated, then we return null 
  getInverse <- function() m
  
  ## 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If we've previously cached the inverse of the matrix
  ## m will NOT be null
  if(!is.null(m)) {
    ## previously calculated and cached. Return the cached value.
    message("getting cached data")
    return(m)
  }
  
  ## the inverse has not been calculated. Calculate it now.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


testCode <- function() {
  ## build an invertable matrix
  v<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
  
  ## make sure it built properly
  print("matrix")
  print(v$get())
  cat("\n")
  
  print("inverse is null")
  print(v$getInverse())
  cat("\n")
  
  ## init the cache
  print("inverse is null. First time attempting. Inverse will be calculated.")
  print(cacheSolve(v))
  cat("\n")
  
  ## call it again to make sure it was retrieved from the cache
  print("Retrieved from cache")
  print(cacheSolve(v))
  cat("\n")
  
  ## verify that we really did calculate the inverse by making sure we got an identity matrix
  print("Should return identity matrix")
  print(cacheSolve(v) %*% v$get())
  cat("\n")
  
}

