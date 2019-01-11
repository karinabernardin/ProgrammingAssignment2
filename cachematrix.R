## This file contains two functions, 1.'makeCacheMatrix' and 2.'cacheSolve'. 
## 1. 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
## 2. 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
##
## A brief example of use:
## x = matrix(c(3,4,2,5,8,2,4,9,15),3,3)
## list_returned <- makeCacheMatrix(x)
## inverse_returned <- cacheSolve(list_returned) 


# Creates a special "matrix" object that can cache the inverse of the input matrix 'x'. Returns the 
# list (special "matrix" object) 
makeCacheMatrix <- function(x = matrix()) { 
        
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inversematrix) inv <<- inversematrix
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

# Computes the inverse of the special "matrix" returned by 'makeCacheMatrix'. The argument 'x' is 
# the list returned by 'makeCacheMatrix'. Returns the inverse matrix.
cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv # Returns a matrix that is the inverse of the argument 'x' of 'makeCacheMatrix'
        
}
