## This function calculates the inverse of a matrix and when it's needed, first look
## if the inverse is already cached. In that case it returns the cache value, otherwise 
## the inverse is calculated and stored as cache (similar to a singleton pattern)

## This function creates the matrix and its getters and setters. And here is saved the inversed matrix
## cached
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of a Matrix, if the inverse is already solved it returns the 
## cached value 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}