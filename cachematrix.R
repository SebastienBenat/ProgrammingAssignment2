## These functions are used to calculate and store to cache a matrix and its 
## inverse matrix. So these data could be used without having to compute again
## inverse matrix each time

## The function makeCacheMatrix gets a matrix as input to store it to cache and
## define access functions to the matrix and its inverse (get / set for matrix 
## and inverse). Matrix and inverse are stored to cache by using <<- operator.
## Each time makeCacheMatrix function is called, inverse matrix is deleted to
## to ensure consistency between matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  #reset inverse matrix
  invMatrix <- NULL
  
  #set the matrix in cache and reset inverse matrix in cache
  set <- function(y) {
    x <<- y
    invMatrix  <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve takes the list creted by makeCacheMatrix function
## and return the inverse matrix of matrix used as arg in makeCacheMatrix. If 
## inverse Matrix has already been calculated, function use cache to get it, 
## otherwise function will recalculate it and use 'setinverse' function define  
## previously to store it in cache

cacheSolve <- function(x, ...) {
  ## check if inversa matrix stored in cache
  inv <- x$getinverse()
  
  ## if yes, get it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise calculate it and store it in cache
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
}
