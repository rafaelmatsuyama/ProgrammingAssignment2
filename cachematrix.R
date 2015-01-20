## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function holds the values of the matrix and the cached value, along with the getter and setter methods.
makeCacheMatrix <- function(x = matrix()) {
        ## Initial value for the cache
        m <- NULL
        
        ## This set function stores the matrix value and resets the cache value because a new matrix may have been set.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Simple get function.
        get <- function() x
        
        ## Simple set function.
        setinverse <- function(inverse) m <<- inverse
        
        ## Simple get function.
        getinverse <- function() m
        
        ## This list creates a sort of mapping of methods available externally via cacheSolve in this case.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function contains the logic to check is there is a cache value or not for a matrix. If there is, I return
## the cache value, otherwise, the inverse of the matrix is calculated, the cache is updated with this new value
## and the inverse matrix is returned.
cacheSolve <- function(x, ...) {
        ## Fetching the cache value.
        m <- x$getinverse()
        
        ## If it isn't null (meaning that there exists a valid cache value), sends a message "getting cached data"
        ## and returns the cache value (the stored inverse matrix).
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If there isn't a valid cache value, get the current matrix value...
        data <- x$get()
        
        ## ... and calculate the inverse of the matrix,
        m <- solve(data, ...)
        
        ## And update the cache value with the result, to avoid further unnecessary computation.
        x$setinverse(m)
        
        ## And it returns the inverse matrix.
        m
}
