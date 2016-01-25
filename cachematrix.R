# Initializes (global) variables set to NULL

inverseMatrixCache <- NULL
m <- NULL

##   This function creates a special "matrix" object
##   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        # Takes a matrix 'x', makes m equal to x and stores its inverse in a variable
        # called 'inverseMatrixCache' that changes the global value
        m <<- x 
        inverseMatrixCache <<- solve(m)
  
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## If inverseCache is not null and the matrix 'x' is 'identical as the one
        ## used in makeCacheMatrix (i.e. 'm') then returns the inverse of matrix 'x'
        
        
        if (is.null(inverseMatrixCache) == FALSE && identical(x,m)) {
                
                y <<- inverseMatrixCache
                
                } 
        
        else {
                
                m <<- x
                y <<- solve(m)
        
                }

        y
  
}
