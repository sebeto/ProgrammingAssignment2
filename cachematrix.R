## Caching the Inverse of a matrix
##
## makeCacheMatrix() and cacheSolve() are used to implement a caching mechanism
## for the inverse of a matrix rather than computing it repeatedly
##
## Usage: 
##     # Create an invertible matrix
##     m1 = rbind(c(1, -1/4), c(-1/4, 1))
##     # Create a new CacheMatrix object
##     y <- makeCacheMatrix(m1)
##     # Store the inverse of m1 in i1
##     i1 <- cacheSolve(y)
##     # Store the inverse of m1 in i2. As the inverse has already been calculated, 
##     # the cached inverse is used
##     i2 <- cacheSolve(y)

###################################################################################
## makeCacheMatrix
## Creates a CacheMatrix object allowing for the caching of the inverse of a matrix
###################################################################################

makeCacheMatrix <- function(x = matrix()) {
    # m is used to store the cached inverse
    m <- NULL
    
    # Sets a new matrix object, and resets the inverse to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Returns the matrix stored in the CacheMatrix object
    get <- function() x
    
    # Sets the cached inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    # returns the inverse of the matrix,
    # or NULL if the inverse hasn't been computed yet
    getinverse <- function() m
    
    # List of components of the CacheMatrix object
    # These functions are used to get/set the matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###################################################################################
## cacheSolve
## Returns the inverse of the matrix stored in a CacheMatrix object
##  - If the inverse has already been calculated, the cached value is returned
##  - If the inverse hasn't been calculated yet, it is calculated and cached in the CacheMatrix object
##  - cacheSolve takes the same arguments as solve()
###################################################################################

cacheSolve <- function(x, ...) {
    # Gets the inverse of the matrix if it has already been calculated
    # NULL if the inverse hasn't been calculated yet
    m <- x$getinverse()
    
    # If the inverse has already been calculated, returns it
    if(!is.null(m)) {
        # Display a message to inform the user that the cached inverse has been used
        message("getting cached data")
        return(m)
    }
    
    # gets the actual matrix to invert
    data <- x$get()
    
    # uses "solve" to invert the matrix
    m <- solve(data, ...)
    
    # stores the calculated inverse in the CacheMatrix object
    # to speed up subsequent calls
    x$setinverse(m)
    
    # returns the calculated inverse
    m
}