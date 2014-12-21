#   makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse. cacheSolve: This function computes the inverse of 
# the special "matrix" returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed), then

makeCacheMatrix <- function(x = matrix()) {
    #m is the inverse variable is set to NULL 
    m <- NULL 
    # function to return value of the original matrix
    get <- function() x 
    # function to set new matrix and reset inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #setminverse (Set matrix inverse) is called by cacheSolve() the first time
    setinverse <- function(solve) m <<- solve
    # getminverse (get matrix inverse) return the cached value to cacheSolve()
    getinverse <- function() m
    # Each time makeCacheMatrix() is called we make a new object.
    # This is a list of internal methods .       
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#   cacheSolve: should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with 
# the solve function in R. For example, if X is a square invertible matrix, 
# then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # We get the inverse matrix to see if is cached
    m <- x$getinverse()
    # if is cached we return    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #else we cache with setminverse
    data <- x$get()
    m<- solve(data, ...)
    x$setinverse(m)
    m
}


