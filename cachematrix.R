## By using the makeCacheMatrix and cacheSolve functions, you can
## not only invert a matrix but also avoid recalculation of a
## previously calculated inverse. The larger your matrix, the more
## processing time you save by inverting once and retrieving the
## cached matrix subsequently.


## The makeCacheMatrix function accepts an invertible matrix and
## returns a list that can be used by the cacheSolve function
## (which see) to invert the matrix and cache the result--or, if
## the inverse was previously calculated, to retrieve the inverse
## from the cache.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setMI <- function(solve) mi <<- solve
        getMI <- function() mi
        list(set = set, get = get,
             setMI = setMI,
             getMI = getMI)
}


## The cacheSolve function accepts a list from the makeCacheMatrix
## function (which see) and determines whether the inverse has
## already been calculated. If so, cacheSolve retrieves the
## inverse from the cache; if not, cacheSolve uses solve() to
## invert the matrix.

cacheSolve <- function(x, ...) {
        mi <- x$getMI()
        if(!is.null(mi)) {
                message("retrieving cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setMI(mi)
        mi
}
