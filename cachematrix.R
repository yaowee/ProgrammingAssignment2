## makeCacheMatrix function creates a matrix object out of a matrix 'x' that
## contains getters and setters functions for the value of the matrix and also
## the inverse of the matrix.
## cacheSolve is a function that takes the matrix object created by makeCacheMatrix
## function and tries to calculate the inverse of the matrix object if the inverse 
## of the object is not calculated before and cached.

## Creates an a matrix, x, which is the input argument. Function has methods 
## that returns the inverse of matrix, x. The function contains is a list of 
## functions: 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ##variable that caches the inverse of matrix x
    inv <- NULL
    ## Set the matrix and resets the cached inverse matrix to NULL
    set <- function(y = matrix()){
        x <<- y
        inv <<- NULL
    }
    ## Returns the matrix
    get <- function() x
    ## Sets the inverse of the matrix in variable inv
    setinv <- function(solve) inv <<- solve
    ## Gets the inverse of the matrix
    getinv <-function() inv
    ## List of functions in this function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Functions returns the inverse of a matrix 'x' by first attempting to return 
## any non-null inverse matrix value that is cached. If the inverse matrix value
## is not cached, the inverse of matrix 'x' is calculated and cached before
## the value of the inverse matrix is return.

cacheSolve <- function(x, ...) {
    ## Gets the inverse from the matrix object
    inv <- x$getinv()
    ## if the inverse is cached, return the cached inverse matrix
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ## get the matrix data
    data <- x$get()
    ## Calculate the inverse of the matrix 'x'
    inv <- solve(data, ...)
    ## cache the inverse value of matrix 'x'
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
