## These 2 functions are used to cache the inverse of a matrix and re-use it (without recalculating it)
## in case it's requested and hasn't been changed


## makeCacheMatrix function is creating a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

    inverseMatrix <- NULL

    ## Function for setting the matrix
    setMatrix <- function() {
        x <<- y
        inverseMatrix <<- NULL
    }

    ## Function for getting the matrix
    getMatrix <- function() x

    ## Function for setting the inverse matrix
    setInverseMatrix <- function(z) inverseMatrix <<- z

    ## Function for getting the inverse matrix
    getInverseMatrix <- function() inverseMatrix

    ## Create the special matrix object
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## cacheSolve function checks if the inverse matrix has been alread calcuated and if not it calcualtes the inverse.
## If the inverse matrix has already been calculated, then it retrieves it from the cache and shows it along with a relevant mesage.

cacheSolve <- function(x, ...) {

    ## Get the inversed matrix
    inverseMatrix <- x$getInverseMatrix()
    
    ## Check if the inverse matrix has already been calcualted.
    ## If it has, return a relevant message.
    if(!is.null(inverseMatrix)) {
          message("Getting matrix from cache!")
    } else { ## If it hasn't, get the matrix, calucate the inverse matrix and set it in the cache for later use.
        matrixData <- x$getMatrix()
        inverseMatrix <- solve(matrixData, ...)
        x$setInverseMatrix(inverseMatrix)
    }
    
    ## Return the inverse matrix
    inverseMatrix
}