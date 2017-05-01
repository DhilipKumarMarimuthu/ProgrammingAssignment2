## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solveMatrix) invMatrix <<- solveMatrix
        getInverseMatrix <- function() invMatrix
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverseMatrix()
        if(!is.null(invMatrix)){
                message("getting cached inverse matrix data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data)
        x$setInverseMatrix(invMatrix)
        invMatrix
}
