## Put comments here that give an overall description of what your
## functions do

## This function creates a Matrix Object
# For simplicity, from now on, the matrix is referred to as "M", and its inverse, as "iM"

makeCacheMatrix <- function(M = matrix()) {
    iM <- NULL
    # This function coerces M to be the given "y" matrix and resets the inverse matrtix "iM"
    set <- function(y) {
        M <<- y
        iM <<- NULL
    }
    # This function simply returns the matrix M
    get <- function() M
    # This function coerces iM to be the given inverse matrix "inverse" 
    setinverse <- function(inverse) iM <<- inverse
    # This function simply returns the inverse matrix "iM"
    getinverse <- function() iM
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the given Matrix Object

cacheSolve <- function(M, ...) {
    # Return a matrix that is the inverse of 'M'
    iM <- M$getinverse()
    if(!is.null(iM)) {
        message("getting cached data")
        return(iM)
    }
    
    # Get the matrix first
    data <- M$get()
    d <- dim(data)
    # Check the matrix is square (not really requested but...)
    if (d[1] != d[2]){
        return(iM)
    } 
    # If the matrix is square, calculate the inverse
    else{
        # Create a diagonal matrix of the same dimension as M
        unitM = diag(d[1])
        iM <- solve(data,unitM)
        M$setinverse(iM)
        iM
    }
}