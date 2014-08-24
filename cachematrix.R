## Creates a matrix object and caches it's inverse.  Results, for subsequent 
## inverse calls using the same matrix value/object are retreived from the cache.

# Gets the inverse of a given matrix
makeCacheMatrix <- function(x = matrix()) {
    i  <- NULL
    set  <- function(y){
        x <<- ymat <- matrix
        i <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) i  <<- inverse
    getinverse  <- function() i
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

## Checks if the inverse has already been calculated and saved to cache.  If so, 
##cached value will be retrieved.  If not, then the inverse is calculated, the 
##result is cached for future lookups, and the result is also sent back to the 
##calling function.


cacheSolve <- function(x, ...) {
    i  <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setinverse(i)
    i
}