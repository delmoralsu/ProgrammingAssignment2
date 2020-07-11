## This pair of functions allow to cache the inverse of a matrix
## and return it when called. It also allows for recalculation, and states so.

## This first function will create the special matrix object that
## allows to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Placeholder for eventual inverse matrix
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix.
## If the inverse has already been calculated, 
## it returns the cached data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## If the inverse has already been calculated, return
  ## it and notify the user
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## If the inverse has not been calculated, do so and
  ## store it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

