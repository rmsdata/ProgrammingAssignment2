##           Caching the Inverse of a Matrix
##          ---------------------------------

## Below are two functions that are used to create a special "matrix" object 
## and cache's its inverse. 

## Assumption: The matrix supplied is always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The special "matrix" object is a list containing a function to
##  1. set the value of the special "matrix"
##  2. get the value of special "matrix" object
##  3. set the inverse of a square matrix
##  4. get  the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL
        
        ## sets the value of the special "matrix" object 
        set <- function(y = matrix()) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        ## gets the value of special "matrix" object
        get <- function() x
        
        ## sets the inverse of a square matrix 
        setInverse <- function(solve) inverseMatrix <<- solve
        
        ## gets  the inverse of a square matrix
        getInverse <- function() inverseMatrix
        
        ## returns the list 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The function, cacheSolve returns the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## get the inverse from the cache
        inverseMatrix <- x$getInverse()
        
        ## If the inverseMatrix is not NULL and has already been calculated 
        ## (and the matrix has not changed), then return the inverse from the cache.
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        
        ## If the inverseMatrix is NULL (or the matrix has changed) then compute inverse and return
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        
        ## return the inverse 
        inverseMatrix
}



# source("cachematrix.R")
# m <- matrix(c(1,1,1,3,4,3,3,3,4), nrow = 3, ncol = 3)
# mm <- makeCacheMatrix(m)
# mm$get()
# mm$getInverse()  ## returns  null
# cacheSolve(mm)   ## computes inverse
# cacheSolve(mm)    ## uses cache
# mm$getInverse()  
# mm$getInverse() 
