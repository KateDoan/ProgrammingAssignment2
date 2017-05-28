## makeCacheMatrix and cacheSolve calculate and cache the inverse of a matrix 
## to save computation time for repeated calls of the same matrix's inverse

makeCacheMatrix <- function(x = numeric()) {
    ## makeCacheMatrix creates a special matrix which can cache its inverse
    ## The special created by makeCacheMatrix is a list of functions to:
    ## - set the value to the matrix
    ## - get the value of the matrix
    ## - set the value of the inverse matrix
    ## - get the value of the inverse matrix
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinv <- function(inv) {
        i <<- inv
    }
    
    getinv <- function() {
        i
    }
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...){
    ## cacheSolve compute the inverse of the matrix returned by makeCacheMatrix
    
    ## If the inverse has been calculated and the matrix has not changed,
    ## this function will retrieve the inverse matrix from the cache
    i <- x$getinv()
    if(!is.null(i)){
        message("Getting the inverse matrix from cached data")
        return (i)
    }
    ## Otherwise, the function will calculate and cache the inverse matrix
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
