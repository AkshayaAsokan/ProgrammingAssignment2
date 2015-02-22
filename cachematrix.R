# The following are the functions that cache the inverse of a matrix.
# 
# 'makeCacheMatrix' creates a special matrix object that can cache its inverse.
# It contains the following functions:
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   set the cached value (matrix inverse)
# getInverse     get the cached value (matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setMatrix <- function(a) {
                x <<- a
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        getInverse <- function() {
                cache
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data)
        x$cacheInverse(inverse)
        inverse
}
