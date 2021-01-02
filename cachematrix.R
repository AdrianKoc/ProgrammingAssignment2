# makeCacheMatrix function creates a special matrix object
# function can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        minverse = NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minverse <<- inverse
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve function computes inverse of matrix from makeCacheMatrix 
# If inverse is already calculated and matrix is not changed,
# cacheSolve will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("returning cached data")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data, ...)
        x$setinverse(minverse)
        minverse
}
