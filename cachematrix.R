## Sometimes it makes more sense to cache data rather than repeatedly calculate it.
## The functions below create an object that stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  reciprocal <- NULL
        set <- function(y) {
                x <<- y
                reciprocal <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) reciprocal <<- inverse
        getinverse <- function() reciprocal
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		reciprocal <- x$getinverse()
		if (!is.null(reciprocal))  {
			message("getting cached data")
			return(reciprocal)
		}
		data <- x$get()
		reciprocal <- solve(reciprocal, ...)
		x$setinverse(reciprocal)
		reciprocal

}
