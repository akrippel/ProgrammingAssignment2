## Put comments here that give an overall description of what your
##Setting and caching matrices

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setINV <- function(inverse) m <<- inverse
        getINV <- function() m
        list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)
}


cacheSolve <- function(x, ...) {
        m <- x$getINV()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setINV(m)
        m
}
