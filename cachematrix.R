## The first of following two main functions
## "makeCacheMatrix" creates a special "matrix"
## that can cache its inverse.
## The second function "cacheSolve" returns the
## inverse of the matrix. Uses cache if that
## matrix inverse is already computed.

## The goal, is to increase efficiency for the
## possible future computations



## Following function makeCacheMatrix
## includes 4 inner functions that sets
## the value of matrix (1), and inverse (2);
## also gets the value of the matrix (3), and
## inverse (4). The returns a list with all the
## values, setters & getters

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## Write a short comment describing this function
## Following function cacheSolve computes the
## inverse of that speacial "matrix" returned by
## first function. If inverse is already calculated
## then cachesolve retrieves the inverse from
## the cache

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
