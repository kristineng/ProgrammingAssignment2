## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()                  ## attempt to get the cached inverse matrix
    if(!is.null(inv)) {                    ## check if the inverse is already cached
        message("getting cached data")     ## if true, display message
        return(inv)                        ## return the cached inverse matrix
    }
    data <- x$get()                        ## get the matrix to be inverted
    inv <- solve(data, ...)                ## compute the inverse of the matrix
    x$setinverse(inv)                      ## set the inverse in cache
    inv                                    ## return the inverse matrix
}
