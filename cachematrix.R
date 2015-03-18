#-----------------------------------------------------
## cachematrix.R
## Author: Harold Trammel
## Purpose: These two functions cache and provide the inverse of a matrix for future use.  
## Usage:  The "makeCacheMatrix" function must be called first, but 
##         the cacheSolve is used for subsequent inversions.  
#-----------------------------------------------------

## Stores a copy of the inverse of matrix "x". Used by cacheSolve function/
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

## Return a matrix that is the inverse of 'x'.  If the inverse of 'x'
## has been stored return that, otherwise invert 'x' and cache it.
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