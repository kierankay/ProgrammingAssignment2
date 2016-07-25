## The following functions reduce the amount of time to obtain the inverse value of 
## a matrix by calculating, and then caching that inverse value

## Create an object that is a list containing a series of functions to:
## store the matrix 
## retrieve the matrix 
## calculate the inverse of the matrix, then cache it
## retrieve the cached inverse value of the matrix

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


## Try and retrieve any cached inverse value of matrix
## otherwise calculate and cache its inverse value.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
