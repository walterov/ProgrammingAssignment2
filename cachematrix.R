## The two functions defined below cache the inverse of a matrix
## One function "makeCacheMatrix" caches the original and inverse matrices
## the other "cacheSolve" calculates the inverse of the matrix if the original matrix is new or has changed

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##      Input: an invertible matrix
##      Output: a list with functions to set/get the input matrix and set/get the inverse matrix
##      Remarks: Both the input matrix and inverse matrix are cached within the environment where the functions are defined

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse matrix to NULL whenever the function is called
        ## this prevents a staled inverse matrix is kept in catche
        i <- NULL
        
        ## Set function for the input matrix
        set <- function(y) {
                x <<- y
                ## Set the inverse matrix to NULL whenever the function is called
                ## this prevents a staled inverse matrix is kept in catche
                i <<- NULL
        }
        
        ## Get function for the input (original) matrix
        get <- function() x
        
        ## Set function for the inversed matrix
        setinverse <- function(inv) {
                i <<- inv                
        } 
        
        ## Get function for the inversed matrix
        getinverse <- function() i
        
        ## Create a list that contains all the set/get operations (functions)
        ## defined in this function (makeCacheMatrix)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             
##      Input: a list with functions to set/get the cached input matrix and set/get the cached inverse matrix.
##             this list is the output from "makeCacheMatrix"
##      Output: a matrix that is inverse of the original matrix
##      Remarks: If the inverse has already been calculated (and the matrix has not changed), 
##               then the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Use 'x' to retrieve the cached inversed matrix
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                ## Return a inverse matrix 
                return(i)
        }
        
        ## If nothing in the cache proceed to retrieve the matrix and calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        
        ## Store in cache the inverse matrix
        x$setinverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i        
}


## Test
m <- matrix(1:4, nrow = 2, ncol = 2)
l<-makeCacheMatrix(m)
cacheSolve(l)
cacheSolve(l)
m <- matrix(5:8, nrow = 2, ncol = 2)
l$set(m)
cacheSolve(l)
cacheSolve(l)
