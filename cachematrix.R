## JH R Programming Assignment 2  Week 3
## Caching the Inverse of a Matrix

## This function creates a special "matrix" 
##      object, and a list
##      containing a functions to 
##      set the value of a matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
        s <- NULL
        
        ##Set and cache the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        ##Get (returns) the value of the matrix
        get <- function() x
        
        ##Set the value of the inverse
        ##The inverse will be set by the cacheSolve function
        ##DO NOT set the value directly
        setinverse <- function(solve) s <<- solve
        
        ##Get the value of the inverse
        getinverse <- function() s
        
        ##Create the list, each element named for
        ##its corresponding function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## ----------------------------------------------------
## JH R Programming Assignment 2  Week 3
## Caching the Inverse of a Vector

## This function will return a matix that is 
##      the inverse of x, the special matrix created 
##      by makeCacheMatrix. If the inverse has 
##      already been computed (and the matrix 
##      has not changed), then the function will
##      retrieve the inverse from the cache.
## NOTE: This will only work on matrices which 
## are invertible.

cacheSolve <- function(x, ...) {
        ##Get the value of existing inverse(may be NULL)
        s <- x$getinverse()
        
        ##If the inverse has already been calculated, 
        ##print message & the cached data
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        
        ##Get matrix, invert it & set the inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        
}