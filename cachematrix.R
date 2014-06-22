## Two functions which create a matrix (must be square), 
##        and then execute and cache the inverse of the matrix


##makeCacheMatrix creates a list of 4 sub-functions which are used to set and retrieve
##      a matrix and it's inverted values.

makeCacheMatrix <- function(x = numeric()) {
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


## cacheSolve is a function which executes the caching and return value for the matrix
##      which is initialized in makeCacheMatrix above.  Function will check for a value
##      in "m" and pull from cache rather than recalculating if available.

cacheSolve <- function(x, ...) {
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
