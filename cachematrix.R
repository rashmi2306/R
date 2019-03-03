## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special matrix which is really a list containing a function to set and get the value of matrix and get and set the value of inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}


## The function i.e. cacheSolve returns the inverse of the special matrix obtained from the above function i.e. makeCacheMatrix. And if the inverse has aready been calculated, then this function retrives the value of inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
  	if (!is.null(i)) {
          message("getting cached data")
          return(i)
  	}
 	 data <- x$get()
  	i <- solve(data, ...)
  	x$setinverse(i)
  	i
}
