## Lexical scoping assignment 

##   This function creates a special vector that contains a list of functions 
##   set the value of the vector
##   get the value of the vector
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function calculates the inverse of the special "vector"
# created with the above function. However, it first checks to see if
# the inverse has already been calculated. If so, it gets the inverse from
# the cache and skips the computation. Otherwise, it calculates the
# inverse of the data and sets the value of the inverse in the cache via
# the setinverse function.

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

