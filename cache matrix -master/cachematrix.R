## Put comments here that give an overall description of what your
## functions do

## create a cache matrix and cache its mean

makeCacheMatrix <- function(x = matrix()) {
        
        cacheInverse <- NULL
        set <- function (y){
                x<<- y
                cacheInverse<<- NULL
        }
        
        get <- function () x
        setCacheInverse <- function (inverse) cacheInverse <<- inverse
        getCacheInverse <- function () cacheInverse
        
        list(set = set, get = get, setCacheInverse = setCacheInverse, getCacheInverse = getCacheInverse)
}


## take a cache matrix created with the above function.
## check if the inverse is cached, if so it returns it, if not it create it , save it and return it.

cacheSolve <- function(x, ...) {

        cacheInverse <- x$getCacheInverse()
        if (!is.null(cacheInverse)){
                message("getting cached data")
                return(cacheInverse)
        }
        matrix <- x$get()
        cacheInverse <- solve(matrix)
        x$setCacheInverse(cacheInverse)
        return(cacheInverse)
}
