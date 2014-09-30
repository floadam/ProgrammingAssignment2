## these functions calculate the inverse of a square matrix, storing the calculation in a global variable 
## so it doesn't need to be recalculated

## creates 4 helper functions

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list (set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)

}


## runs one of the 4 functions above depending on whether the inverse has been calculated already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}

