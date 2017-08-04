## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Define function
    set <- function(y) {
        x <<- y    
        m <<- NULL
    }
    ## Define function geting the value of the matrix
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    ## Define function setting inverse. 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x) {
    m <- x$getInverse() ## Fetching cached value for inverse
    if(!is.null(m)) { ## Returning if cache is not empty 
        message("getting cached data")
        return(m)
    }
    ## Returning cache after calculating
    data <- x$get()  # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m)  # Cache the result
    m                # Return the inverse
}
