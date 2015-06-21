## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object (wrapper) that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # inverse matrix
  
  # set a new matrix and flags it as changed (cached value of m is reset to NULL)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the current matrix
  get <- function() x
  
  # set the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  # returns the mean of the matrix
  # NB: returns NULL if cacheSolve has not been invoked (inverse matrix not computed)
  getinverse <- function() i
  
  # return special "matrix" object (wrapper)
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) # compute inverse matrix
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
