##  This pair of functions that cache the inversion of a matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- null
    set <- function(y) {
        x <<- y
        inv <<- null
    }
    get <- function() x
    setinversion <- function(inversion) inv <<- inversion
    getinversion <- function() inv
    list(set = set, get = get, 
         setinversion = setinversion,
         getinversion = getinversion)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSovle <- function(x, ...) {
    inv <- x$getinversion()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    inv
}
