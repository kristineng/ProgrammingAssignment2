## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
cacheMatrix <- function(matrix = matrix()) {
    invMat <- NULL  ## hold the inverse matrix when calculated
    assignMatrix <- function(newMatrix) {  ## set a new matrix in the parent environment
        matrix <<- newMatrix
        invMat <<- NULL  ## because the matrix has changed
    }
    retrieveMatrix <- function() matrix  ## retrieve the current matrix
    cacheInverse <- function(inverse) invMat <<- inverse  ## set the cached inverse matrix
    getInverse <- function() invMat  ## get the cached inverse matrix
    list(set = assignMatrix, get = retrieveMatrix, cacheInverse = cacheInverse, getInverse = getInverse)  ## Return a list of functions
}
solveMatrix <- function(matrixObj, ...) {
    invMat <- matrixObj$getInverse() 
    if(!is.null(invMat)) {
        message("cached result")
        return(invMat)
    }
    mat <- matrixObj$get()  ## get the matrix to be inverted
    invMat <- solve(mat, ...) 
    matrixObj$cacheInverse(invMat) 
    invMat 
}
