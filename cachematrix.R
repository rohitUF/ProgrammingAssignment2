## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Following are a pair of functions that cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL

    ## Use a method to set the matrix
    setMatrix <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Getter Method for matrix
    getMatrix <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix method above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cacheSolve method should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- mat$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- mat$getMatrix()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    mat$setInverse(m)

    ## Return the matrix
    m
}
