# Couresra's R Programming Assignment 2 on Lexical Scoping. 

# ASSUMPTION: INPUT MATRIX IS ALWAYS INVERTIBLE. 

# In the below function the following functions are defined. 
# Get, Set Matrix and get Inverse, set Inverse (xirtam is inverse)
makeCacheMatrix <- function(x = matrix()) {
    xirtam <- NULL
    set <- function(y) {
      x <<- y
      xirtam <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xirtam <<- inverse
    getinverse <- function() xirtam
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


# Getting inverse of the matrix from cache
cacheSolve <- function(x, ...) {
    xirtam <- x$getinverse()
    # Gets inverse after the first run, checks if it is the first run. If it is first run, 
    # the inverse is computed using solve function, else inverted matrix is obtained from cache. 
    if(!is.null(xirtam)) {
      message("getting cached data")
      return(xirtam)
    }
    data <- x$get()
    # Matrix inversion is done using solve function in R
    xirtam <- solve(data)
    x$setinv(xirtam)
    xirtam
}
