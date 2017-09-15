## makeCacheMatrix creates a special "matrix", which is basically a list containing a function to 
## set the value of the matrix, 
## get the value of the matrix, 
## set the value of the inverse of the matrix and
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the special matrix created with the makeCacheMatrix function
## First Checks if the inverse has already been calculated
## If so, cacheSolve retrieves the inverse from the cache
## Else, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv                           ## Returning the inverse matrix of x
}