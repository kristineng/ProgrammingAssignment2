## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
cacheMatrix <- function(matrix = matrix()) {
    inv <- NULL  ## hold the inverse matrix when calculated
    assignMatrix <- function(newMatrix) {  ## set a new matrix in the parent environment
        matrix <<- newMatrix
        inv <<- NULL  ## because the matrix has changed
    }
    retrieveMatrix <- function() matrix  ## retrieve the current matrix
    cacheInverse <- function(inverse) inv <<- inverse  ## set the cached inverse matrix
    getInverse <- function() inv  ## get the cached inverse matrix
    list(set = assignMatrix, get = retrieveMatrix, cacheInverse = cacheInverse, getInverse = getInverse)  ## Return a list of functions
}
solveMatrix <- function(matrixObj, ...) {
    inv <- matrixObj$getInverse() 
    if(!is.null(inv)) {
        message("cached result")
        return(inv)
    }
    mat <- matrixObj$get()  ## get the matrix to be inverted
    inv <- solve(mat, ...) 
    matrixObj$cacheInverse(inv) 
    inv 
}
