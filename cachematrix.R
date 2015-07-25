## My functions create a special "matrix" object that stores a matrix and
## caches its inverse


## This function 'makeCacheMatrix' creates a special "matrix" which is
## really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }                                       
        get <- function() x                     
        setinverse <- function(slv) s <<- slv   
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function named 'cacheSolve' returns the inverse of the special "matrix"
## created by 'makeCacheMatrix'. It just retrieves it from the cache and
## and returns it if it has already been calculated. If not, it computes it,
## caches it and returns its value.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                return(s)
        }                                                       ## If so, finds it in the cache and returns it.
        data <- x$get()         
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
