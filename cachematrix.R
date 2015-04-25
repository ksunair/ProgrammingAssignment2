## Matrix inversion is ususally a costly computation and using caching we can improve the performance
## by calculating the inverse once and as along as the matrix stay the same, we can return the cached 
## result. This is achived by using two methods one to set the cache and one to solve it.

## makeCacheMatrix creates the functions to set the matrix, inverse it, calculate the inverse and put
## in the cache. This also exposes the methods other consumers of the function can call on to perform
## operations on the cache matrix. Following methods are exposed
## set        <- set the matrix.
## get        <- get the matrix.
## setinverse <- creates inverse of the matrix.
## getinverse <- returns the current inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' return by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then cacheSlve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
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

