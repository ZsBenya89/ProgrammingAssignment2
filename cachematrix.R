## This function will create the inverse of every matrix and store the result 
## till the input matrix is unchanged. It is prevent unnesessary computations, 
## therefore the runs will be optimized (makes the runs faster). Instead of 
## recomputing, the result will be read from the cache

## The makeCacheMatrix function is initalizing the matrix, defining the behaviors
## and the functions for objects. 
## In the set element the input matrix is assigned to the parent enviroment and 
## clear all value in the "inv" object. 
## Next, the getter and setter elements are defined, and finally the created 
## functions are assigned as a list, to use these sub functions in the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The cacheSolve function is populating or retrieving the inverse of the input 
## matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
