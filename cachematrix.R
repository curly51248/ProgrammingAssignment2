## Put comments here that give an overall description of what your
## functions do
## Curly51248 for completion of the Programming Assignment #2.
## create two functions that will:
##  1)makeCacheMatrix   create a matrix object that can cache its inverse.
##  2)cacheSolve   This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.   create a matrix object that can cache its inverse.

## Write a short comment describing this function
## makes the matrix ojbect
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## Write a short comment describing this function
# computes the inverse of the matrix returned y makecachematrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
