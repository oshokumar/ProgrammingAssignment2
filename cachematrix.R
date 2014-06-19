##The function makeCacheMatrix here inverts the matrix and stores it in the cache

##This stores the result for the inverse.The inverse is calculated only once. 
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



##This takes the result from the makeCacheMatrix function.It could return either a cached value or the calculated value.

cacheSolve <- function(x, ...) {
             m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
