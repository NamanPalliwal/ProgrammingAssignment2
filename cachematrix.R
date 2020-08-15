## Put comments here that give an overall description of what your
## functions do

## This function passes the given matrix, 'x' to 'get' which in turn taken by
## 'cacheSolve' to find inverse matrix of 'x'

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


## This function finds inverse of matrix, 'x' if 'getInverse'is NULL by taking 
## 'x' from 'get' and finding inverse by function 'solve'  and then gives back
## inverse to 'makeCacheMatrix' through 'setInverse'.
## If 'getInverse' is not NULL, it returns the same value to 'makeCacheMatrix'.

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
