## The following functions calculate the inverse of a matrix and will cache the result
## so that next time the inverse is calculated the cached value is returned 
## instead of repeating the calculation

## This function creates a matrix that can cache it's inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of a matrix. If the matrix has not changed
## then the cached inverse is returned

cachesolve <- function(x, ...) {
   inv <- x$getInverse()
   if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}