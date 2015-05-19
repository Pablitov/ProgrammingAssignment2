## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(M = matrix()) {
    iM <- NULL
    
    set <- function(y) {
        M <<- y
        iM <<- NULL
    }
    
    get <- function() M
    
    setinverse <- function(inverse) iM <<- inverse
    
    getinverse <- function() iM
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
    iM <- M$getinverse()
    if(!is.null(iM)) {
        message("getting cached data")
        return(iM)
    }
    data <- M$get()
    d <- dim(data)
    if (d[1] != d[2]){
        return(iM)
    } 
    else{
        unitM = diag(d[1])
        iM <- solve(data,unitM)
        M$setinverse(iM)
    }
    iM    
}
