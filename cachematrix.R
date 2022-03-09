## Put comments here that give an overall description of what your
## functions do

## Here the functions are used to create a special object that stores a matrix 
## and caches its inverse. makeCacheMatrix makes a special "matrix", which is a
## list containing a function to: set the value of the matrix, get the value of 
## the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Here the function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated (
## and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
}
