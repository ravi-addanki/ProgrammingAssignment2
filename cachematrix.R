## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function transfers the input to internal storage(I.S.) x,
## sets I.S. inv as null, and returns the vector of 4 functions.
## get reads the value of x which represents the original matrix
## getinv reads the value of inv which represents inverse matrix
## set can be used to change x and setinv can be used change inv

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    list( set = set, get = get, 
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function

## This function checks if inverse of matrix is NULL
## If it is NULL, it means that the inverse is not yet computed
## and it calculates the inverse and returns this value
## If inverse is not NULL, this function simply returns inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- x$getinv()
    if(is.null(inverseMatrix)) {
        data <- x$get()
        inverseMatrix <- solve(a = data, ...)
        x$setinv(inverseMatrix)
    } else cat("Inverse matrix returned from Cache! \n")
    inverseMatrix
}
