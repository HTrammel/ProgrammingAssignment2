#-----------------------------------------------------
## cachematrix.R
## Author: Harold Trammel
## Purpose: To cache and provide the inverse of a matrix for future use.  
## Usage:  
##      Given x is a square matrix...
##      m <- makeCacheMatrix(x)
##      inv <- cacheSolve(m)
##
##      inv <- cacheSolve( makeCacheMatrix(x) ) can be used as well
#-----------------------------------------------------

## Stores a local copy of the inverse of matrix "x". Provides
## four internal functions for use by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    cached_inversion <- NULL  
    set <- function(y) {
        x <<- y           
        cached_inversion <<- NULL  
    }
    get <- function() {
        return (x)       
    }
    setInverse <- function(new_inversion) {
        cached_inversion <<- new_inversion
    }
    getInverse <- function() {
        return (cached_inversion)
    }
    list( set = set
          , get = get
          , setInverse = setInverse
          , getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x'.  
## Function is passed an instance of the makeCacheMatrix() function,
## which provides the set, get, setInverse, and getInverse functions.
## If inverse of 'x' has not been stored, then solves for inverse.
cacheSolve <- function(x, ...) {
    cached_inverse <- x$getInverse()
    if (!is.null(cached_inverse)) {
        # if 
        message ("getting cached data...")
        return (cached_inverse)
    } else {
        data <- x$get()
        cached_inverse <- solve(data)
        x$setInverse(cached_inverse)
        message ("setting cached data...")
        return(cached_inverse)
    }
}
