## This function creates a special "matrix" object that can cache its inverse.
## ...based on makeCacheVector() and cachemean() from Coursera R Programming 

makeCacheMatrix <- function(x = matrix()) {
	## creates a special "matrix" which is really a list containing functions to:	
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the inverse
    ## 4. get the inverse

	if (nrow(x) != ncol(x)) {
		stop("Error in makeCacheMatrix(x) : x must be a square")
	}
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}

## This is the function that returns the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	## Checks to see if the inverse has already been computed. 
	## If so, it returns the cached inverse matrix and skips the new computation.

    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")  ## May want to comment this line out
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m	
}
