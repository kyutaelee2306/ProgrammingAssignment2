## This program creates two functions which will cache the inverse
## of a matrix so that subsquent uses of that inverse matrix can
## be retrieved from the cache rather than be recalcualted in order to
## save time and computation power

## makeCacheMatrix creates an object that allows the setting,getting
## setting the inverse matrix, and getting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  ## initialize
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function gets the inverse of a matrix of the special vector.  if the
## inverse is cached, it uses the cached version and messages that it is
## getting cached data, otherwise it calculates the inverse and stores it
## in cache before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setsolve(m)
	m
}
