## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {    ## define the element 
    inv <- NULL                              ## will hold value of of matrix inverse
    set <- function(y) {                     ##
        x <<- y
        inv <<- NULL
    }
    get <- function() x                      ## returns the value of the matrix argument
    setInverse <- function(inverse) inv<<- inverse ## assigns the value of inv in parent environment
    getInverse <- function() inv    ##get the value of inv where called
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {        ##check if the inv is already cached
        message('geting cached data')##if true, display message
        return(inv)
    }
    data <- x$get()        ##get the matrix to be inverted
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
